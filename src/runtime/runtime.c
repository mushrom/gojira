#include <gojira/runtime/runtime.h>
#include <gojira/parser.h>
#include <gojira/lexer.h>
#include <gojira/parse_debug.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

token_t *eval_function( st_frame_t *frame, list_head_t *tokens ){
	token_t *token = NULL;
	token_t *func = NULL;
	token_t *ret = NULL;
	list_node_t *temp;
	temp = tokens->base;

	if ( temp && temp->data ){
		func = temp->data;
		printf( "[%s] Dumping token list for type %s: ",
				__func__, type_str( func->type ));
		//printf( "[%s] Dumping token list: ", __func__ );

		foreach_in_list( temp ){
			token = temp->data;

			if ( token ){
				printf( "%s", type_str( token->type ));

				if ( token->type == TYPE_SYMBOL ){
					char *name = token->data;
					printf( " \"%s\"", name );
					printf( " (at %p)", frame_find_var( frame, name ));
				}

				printf( " -> " );
			}
		}

		printf( "\n" );
	}

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_QUOTED_TOKEN;
	token = ret->down = calloc( 1, sizeof( token_t ));
	token->type = TYPE_LIST;

	return ret;
}

st_frame_t *eval_loop( st_frame_t *frame, token_t *tokens ){
	token_t expr;
	token_t *cptr;
	st_frame_t *cur_frame;

	memset( &expr, 0, sizeof( token_t ));
	cptr = tokens;

	cur_frame = frame;

	while ( cur_frame ){
		while ( cptr ){
			if ( cptr->type == TYPE_LIST && cptr->down ){
				// Create frameinuation, evaluate tokens in list
				printf( "[%s] Got a list, ret = %p\n", __func__, cptr );

				if ( cptr->down->type == TYPE_SYMBOL && (
						strcmp( cptr->down->data, "define" ) == 0 ||
						strcmp( cptr->down->data, "lambda" ) == 0 )){

					//handle special functions here
					cptr = cptr->next;

				} else {
					if ( cptr->down->type == TYPE_SYMBOL ){
						printf( "[%s] Will be evaluating \"%s\"...\n",
								__func__, (char *)cptr->down->data );
					}

					cur_frame = frame_create( cur_frame, cptr->next );
					cptr = cptr->down;
				}

			} else {
				printf( "[%s] Don't know what to do with token of type \"%s\", "
						"continuing...\n", __func__, type_str( cptr->type ));

				list_add_data( cur_frame->expr, cptr );
				printf( "[%s] Adding token to current expression "
						"list of type \"%s\"\n", __func__, type_str( cptr->type ));
				if ( cptr->type == TYPE_SYMBOL )
					printf( "[%s]\t symbol: \"%s\"\n", __func__, (char *)cptr->data );

				cptr = cptr->next;
			}
		}

		// Evaluate function
		cur_frame->value = eval_function( cur_frame, cur_frame->expr );

		// return from current frame, if cur_frame->ret
		// is not null
		printf( "[%s] Returning to %p\n", __func__, cur_frame->ret );

		if ( cur_frame->last && cur_frame->last->expr && cur_frame->value ){
			list_add_data( cur_frame->last->expr, cur_frame->value );
			printf( "[%s] Returning token to last expression type \"%s\"\n",
					__func__, type_str( cur_frame->value->type ));

		} else if ( !cur_frame->value ){
			printf( "[%s] Warning: Frame returned no value, "
					"something might be broken\n", __func__ );
		}

		cptr = cur_frame->ret;
		cur_frame = cur_frame->last;
	}

	return frame;
}

token_t *eval_tokens( stack_frame_t *st_frame, token_t *tokens ){
	token_t *ret = tokens;
	st_frame_t *temp_frame = frame_create( NULL, NULL );
	list_node_t *foo;

	frame_add_var( temp_frame, "foo", parse_tokens( lexerize( "'()" )));

	eval_loop( temp_frame, tokens );
	foo = list_get_index( temp_frame->expr, 0 );
	ret = foo->data;

	printf( "[%s] Temp frame has return type \"%s\"\n",
			__func__, type_str( ret->type ));

	return ret;
}

st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ret_pos ){
	st_frame_t *ret;

	ret = calloc( 1, sizeof( st_frame_t ));
	ret->last = cur_frame;
	ret->ret = ret_pos;

	ret->value = NULL;
	ret->expr = list_create( 0 );

	return ret;
}

token_t *frame_find_var( st_frame_t *frame, char *key ){
	token_t *ret = NULL;
	list_node_t *temp;
	variable_t *var;

	if ( frame ){
		if ( frame->vars ){
			temp = frame->vars->base;
			foreach_in_list( temp ){
				var = temp->data;

				if ( strcmp( var->key, key ) == 0 ){
					ret = var->token;
					break;
				}
			}

		} else {
			ret = frame_find_var( frame->last, key );
		}
	}
	return ret;
}

variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token ){
	variable_t *new_var;

	if ( !frame->vars )
		frame->vars = list_create( 0 );

	new_var = calloc( 1, sizeof( variable_t ));
	new_var->key = strdup( key );
	new_var->token = token;

	list_add_data( frame->vars, new_var );

	return new_var;
}
