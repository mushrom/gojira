#include <gojira/runtime/syntax.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/shared.h>
#include <gojira/libs/dlist.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

bool has_symbol( token_t *tokens, char *sym ){
	bool ret = false;
	token_t *move;

	move = tokens;
	foreach_in_list( move ){
		if ( move->type == TYPE_SYMBOL && strcmp( shared_get( move->data ), sym ) == 0 ){
			ret = true;
			break;
		}
	}

	return ret;
}

token_t *compile_lambda( stack_frame_t *frame, token_t *args, token_t *tokens ){
	token_t *ret = tokens;
	shared_t *shr;
	char *varname;

	if ( tokens ){
		if ( tokens->type == TYPE_SYMBOL ){
			varname = shared_get( tokens->data );
			if ( !has_symbol( args, varname ) &&
			      //shr = frame_find_var( frame, varname, RECURSE )){
			     ( shr = frame_find_shared_struct( frame, varname, RECURSE )))
			{

				ret = alloc_token( );
				ret->type = TYPE_VARIABLE_REF;
				ret->flags |= T_FLAG_HAS_SHARED;
				ret->next = tokens->next;
				ret->down = NULL;

				//shr = frame_find_shared_struct( frame, varname, RECURSE );
				ret->data = shared_aquire( shr );

				free_token( tokens );
			}

		} else if ( tokens->type != TYPE_QUOTED_TOKEN && tokens->type != TYPE_VECTOR ){
			ret->down = compile_lambda( frame, args, tokens->down );
		}

		ret->next = compile_lambda( frame, args, ret->next );
	}

	return ret;
}

void free_procedure( void *ptr ){
	if ( ptr ){
		procedure_t *proc = ptr;

		//printf( "[%s] Freeing procedure at %p\n", __func__, ptr );
		free_tokens( proc->body );
		free_tokens( proc->args );
		free( proc );
	}
}

void free_vector( void *ptr ){
	if ( ptr ){
		dlist_t *dlst = ptr;
		unsigned i;

		//printf( "[%s] Freeing vector at %p\n", __func__, ptr );

		foreach_in_dlist( i, dlst ){
			//printf( "[%s] Freeing vector element at %p\n", __func__, dlist_get( dlst, i ));
			free_tokens( dlist_get( dlst, i ));
		}
	}
}

token_t *expand_lambda( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = alloc_token( );
	token_t *temp;
	shared_t *shr;

	procedure_t *proc = calloc( 1, sizeof( procedure_t ));;

	if ( tokens_length( tokens ) < 3 ){
		frame->error_call( frame,
				"[%s] Error: expected at least 3 tokens in lambda expression, but have %u\n",
				__func__, tokens_length( tokens ));
		return NULL;
	}

	temp = clone_tokens( tokens->next->next );

	proc->args = clone_tokens( tokens->next->down );
	proc->body = compile_lambda( frame, proc->args, temp );
	shr = shared_new( proc, free_procedure );

	ret->type = TYPE_PROCEDURE;
	ret->data = shr;
	ret->flags |= T_FLAG_HAS_SHARED;
	//ret->data = proc;

	return ret;
}

token_t *expand_vector( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = NULL;
	token_t *foo;

	if ( frame->ptr->down && frame->ptr->down->type == TYPE_LIST ){
		foo = alloc_token( );
		dlist_t *nlist = dlist_create( 0, 0 );
		shared_t *shr = shared_new( nlist, free_vector );
		token_t *temp = frame->ptr->down->down;

		foo->type = TYPE_VECTOR;
		foo->data = shr;
		foo->flags |= T_FLAG_HAS_SHARED;

		foreach_in_list( temp ){
			dlist_add( nlist, clone_token_tree( temp ));
		}

		ret = foo;

	} else {
		frame->error_call( frame, "[%s] Error: Invalid vector syntax\n", __func__ );
	}

	return ret;
}

stack_frame_t *expand_procedure( stack_frame_t *frame, token_t *tokens ){
	stack_frame_t *ret = NULL;
	token_t *move;
	token_t *temp;
	procedure_t *proc;
	char *var_name;
	shared_t *shr;

	if ( tokens->type == TYPE_PROCEDURE ){
		shr = tokens->data;
		//proc = tokens->data;
		proc = shared_get( shr );
		move = tokens->next;
		temp = proc->args;

		//dump_tokens( move );

		foreach_in_list( temp ){
			if ( temp->type == TYPE_SYMBOL ){
				var_name = shared_get( temp->data );

				if ( !move ){
					frame->error_call( frame, "[%s] Error: Have unbound variable \"%s\"\n", __func__, var_name );
					break;
				}

				//printf( "[%s] Adding \"%s\" as %p\n", __func__, var_name, move );
				frame_add_var( frame, var_name, move, NO_RECURSE );
				move = move->next;

			} else {
				frame->error_call( frame, "[%s] Error: expected symbol in procedure definition, have \"%s\"\n",
						__func__, type_str( temp->type ));
				//stack_trace( ret );
			}
		}

		ret = frame;

		ret->expr = ret->end = NULL;

		temp = ext_proc_token( builtin_return_last );
		frame_add_token_noclone( ret, temp );

		ret->ptr = proc->body;

	} else {
		frame->error_call( frame, "[%s] Error: Trying to apply non-procedure as procedure (?!)\n", __func__ );
	}

	return ret;
}

stack_frame_t *expand_procedure_old( stack_frame_t *frame, token_t *tokens ){
	stack_frame_t *ret = NULL;
	token_t *move;
	token_t *temp;

	token_t *args;
	token_t *body;
	char *var_name;
	bool is_tailcall = false;

	if ( tokens->type == TYPE_PROCEDURE ){
		move = tokens->down;

		args = move->next;

		if ( args && args->type == TYPE_LIST ){
			body = clone_tokens( args->next );
			move = tokens->next;
			temp = args->down;

			foreach_in_list( temp ){
				if ( temp->type == TYPE_SYMBOL ){
					var_name = temp->data;

					if ( !move ){
						frame->error_call( frame, "[%s] Error: Have unbound variable \"%s\"\n", __func__, var_name );
						break;
					}

					token_t *add;
					add = clone_token_tree( temp );
					add->down = clone_token_tree( move );

					gc_unmark( add );
					frame_register_token_tree( frame, add );

					body = replace_symbol_safe( body, add, var_name );
					move = move->next;

				} else {
					frame->error_call( frame, "[%s] Error: expected symbol in procedure definition, have \"%s\"\n",
							__func__, type_str( temp->type ));
					//stack_trace( ret );
				}
			}

			if ( frame->last->ptr == NULL && frame->last->status == TYPE_PROCEDURE ){
				ret = frame->last;
				is_tailcall = true;
			} else {
				ret = frame;
			}

			ret->expr = ret->end = NULL;

			temp = ext_proc_token( builtin_return_last );
			frame_add_token_noclone( ret, temp );

			frame_register_token_tree( ret, body );

			for ( temp = body->next; temp; temp = temp->next )
				frame_register_token_tree( ret, temp );

			ret->ptr = body;

			if ( is_tailcall ){
				frame->heap = gc_sweep( frame->heap );
				frame_free( frame );
			}
		}

	} else {
		frame->error_call( frame, "[%s] Error: Trying to apply non-procedure as procedure (?!)\n", __func__ );
	}

	return ret;
}

token_t *expand_syntax_rules( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = NULL;

	token_t *cur;
	token_t *pattern;
	token_t *template;

	token_t *move, *foo;
	bool matched = false;
	int args;
	int len;

	cur = tokens->down;
	len = tokens_length( cur );
	args = tokens_length( tokens );

	if ( len >= 3 ){
		cur = cur->next->next;

		for ( ; cur; cur = cur->next ){
			pattern = cur->down->down;
			template = cur->down->next;

			if ( tokens_length( pattern ) == args ){
				matched = true;
				ret = clone_token_tree( template );

				for ( move = pattern, foo = tokens; move && foo;
						move = move->next, foo = foo->next )
				{
					ret = replace_symbol( ret, foo, shared_get( move->data ));
				}

				gc_unmark( ret );
				frame_register_token_tree( frame, ret );
			}
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected at least 3 tokens, but got %d\n", __func__, len );
	}

	if ( !matched ){
		frame->error_call( frame, "[%s] Error: Could not match syntax pattern\n", __func__ );
	}

	return ret;
}
