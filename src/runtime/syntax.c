#include <gojira/runtime/syntax.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

stack_frame_t *expand_procedure( stack_frame_t *frame, token_t *tokens ){
	stack_frame_t *ret = NULL;
	token_t *move;
	token_t *temp;
	token_t *orig_expr;

	token_t *args;
	token_t *body;
	char *var_name;
	bool is_tailcall = false;

	if ( tokens->type == TYPE_PROCEDURE ){
		move = tokens->down;

		args = move->next;

		if ( args && args->type == TYPE_LIST ){
			body = clone_tokens( args->next );
			move = clone_tokens( tokens->next );
			temp = args->down;

			foreach_in_list( temp ){
				if ( temp->type == TYPE_SYMBOL ){
					var_name = temp->data;

					if ( !move ){
						printf( "[%s] Error: Have unbound variable \"%s\"\n", __func__, var_name );
						break;
					}

					token_t *add;
					add = clone_token_tree( temp );
					add->down = clone_token_tree( move );

					body = replace_symbol_safe( body, add, var_name );
					move = move->next;

				} else {
					printf( "[%s] Error: expected symbol in procedure definition, have \"%s\"\n",
							__func__, type_str( temp->type ));
					stack_trace( ret );
				}
			}

			if ( /* frame->last && */ frame->last->ptr == NULL && frame->last->status == TYPE_PROCEDURE ){
				//printf( "[%s] Have tail call\n", __func__ );
				ret = frame->last;
				is_tailcall = true;

			} else {
				/*
				printf( "[%s] Don't have tail call, status = %s, frame->last->ptr = %p\n",
						__func__, type_str( frame->last->status ), frame->last->ptr );
				dump_tokens( frame->last->ptr, 2 );
				*/
				ret = frame;
			}

			//orig_expr = ret->expr;
			free_tokens( ret->expr );
			ret->expr = ret->end = NULL;
			frame_add_token( ret, ext_proc_token( builtin_return_last ));

			ret->ptr = body;

			//free_tokens( orig_expr );
			if ( is_tailcall ){
				frame_free( frame );
			}
		}

	} else {
		printf( "[%s] Error: Trying to apply non-procedure as procedure (?!)\n", __func__ );
	}

	return ret;
}

token_t *expand_if_expr( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = NULL; 
	token_t *move;
	int len;

	len = tokens_length( tokens );

	if ( len == 4 ){

		move = calloc( 1, sizeof( token_t ));
		move->type = TYPE_IF;
		move->down = frame->ptr->next->next;
		move->next = clone_token_tree( frame->ptr->next );
		ret = move;

	} else {
		printf( "[%s] Error: If statement expected 4 tokens, but got %d\n", __func__, len );
	}

	return ret;
}

token_t *expand_syntax_rules( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = NULL;

	token_t *keywords;
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
		// do stuff
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
			}
		}

	} else {
		printf( "[%s] Error: Expected at least 3 tokens, but got %d\n", __func__, len );
	}

	if ( !matched ){
		printf( "[%s] Error: Could not match syntax pattern\n", __func__ );
	}

	return ret;
}
