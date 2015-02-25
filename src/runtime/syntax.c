#include <gojira/runtime/syntax.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/shared.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

bool has_symbol( token_t *tokens, char *sym ){
	bool ret = false;
	token_t *move;

	move = tokens;
	foreach_in_list( move ){
		if ( move->type == TYPE_SYMBOL && strcmp( move->data, sym ) == 0 ){
			ret = true;
			break;
		}
	}

	return ret;
}

token_t *compile_lambda( stack_frame_t *frame, token_t *args, token_t *tokens ){
	token_t *ret = tokens;
	token_t *temp;
	shared_t *shr;

	if ( tokens ){
		if ( tokens->type == TYPE_SYMBOL ){
			if ( !has_symbol( args, tokens->data ) &&
			      frame_find_var( frame, tokens->data, RECURSE )){

				ret = alloc_token( );
				ret->type = TYPE_VARIABLE_REF;
				ret->next = tokens->next;
				ret->down = NULL;
				//ret->data = frame_find_var_struct( frame, tokens->data, RECURSE );
				//ret->data = frame_find_var_struct( frame, tokens->data, RECURSE );
				shr = frame_find_shared_struct( frame, tokens->data, RECURSE );
				ret->data = shared_aquire( shr );
				//shr->references++;

				printf( "[%s] add binding for \"%s\" here at %p\n", __func__, tokens->data, ret->data );

				free_token( tokens );
			}

		} else if ( tokens->type != TYPE_QUOTED_TOKEN && tokens->type != TYPE_VECTOR ){
			ret->down = compile_lambda( frame, args, tokens->down );
		}

		ret->next = compile_lambda( frame, args, ret->next );
	}

	return ret;
}

token_t *expand_lambda( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = alloc_token( );
	token_t *temp;

	procedure_t *proc = calloc( 1, sizeof( procedure_t ));;

	if ( tokens_length( tokens ) < 3 ){
		frame->error_call( frame,
				"[%s] Error: expected at least 3 tokens in lambda expression, but have %u\n",
				__func__, tokens_length( tokens ));
		return NULL;
	}

	temp = clone_tokens( tokens->next->next );

	proc->args = tokens->next->down;
	proc->body = compile_lambda( frame, proc->args, temp );
	proc->bindings = NULL;

	ret->type = TYPE_PROCEDURE;
	ret->data = proc;

	return ret;
}

stack_frame_t *expand_procedure( stack_frame_t *frame, token_t *tokens ){
	stack_frame_t *ret = NULL;
	token_t *move;
	token_t *temp;
	procedure_t *proc;
	char *var_name;

	if ( tokens->type == TYPE_PROCEDURE ){
		proc = tokens->data;
		move = tokens->next;
		temp = proc->args;

		//dump_tokens( move );

		foreach_in_list( temp ){
			if ( temp->type == TYPE_SYMBOL ){
				var_name = temp->data;

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
					frame_register_token( frame, add );

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

			frame_register_token( ret, body );

			for ( temp = body->next; temp; temp = temp->next )
				frame_register_token( ret, temp );

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

token_t *expand_if_expr( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = NULL; 
	token_t *move;
	int len;

	len = tokens_length( tokens );

	if ( len == 4 ){

		move = alloc_token( );
		move->type = TYPE_IF;
		move->down = clone_tokens( frame->ptr->next->next );
		move->next = frame_register_token( frame, clone_token_tree( frame->ptr->next ));
		gc_unmark( move );

		ret = move;

	} else {
		frame->error_call( frame, "[%s] Error: If statement expected 4 tokens, but got %d\n", __func__, len );
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
					ret = replace_symbol( ret, foo, move->data );
				}

				gc_unmark( ret );
				frame_register_token( frame, ret );
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
