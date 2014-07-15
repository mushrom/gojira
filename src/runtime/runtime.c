#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parser.h>
#include <gojira/lexer.h>
#include <gojira/parse_debug.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

token_t *eval_tokens( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = tokens;
	token_t *temp;
	token_t *move;
	token_t *foo;
	char *name;
	st_frame_t *tempframe;
	
	//tempframe = frame_create( frame, NULL );

	if ( tokens ){
		for ( move = tokens; move; move = move->next ){
			if ( move->type == TYPE_SYMBOL ){
				name = move->data;
				temp = frame_find_var( frame, name );

				if ( temp ){
					ret = temp;

				} else {
					// Error out here, undefined variable
					printf( "[%s] Error: undefined variable \"%s\"\n", __func__, name );
					//ret = NULL;
					ret = move;
				}

			} else if ( move->type != TYPE_LIST ){
				ret = move;

			} else {
				bool builtin = false;
				temp = move->down;

				if ( move->down ){
					if ( temp->type == TYPE_DEFINE_EXPR ){
						builtin = true;

						temp = temp->down;
						token_t *newvar = NULL;
						token_t *setvar = NULL;

						if ( temp->next && temp->next->type == TYPE_SYMBOL ){
							newvar = temp->next;

							if ( temp->next->next ){
								setvar = eval_tokens( frame, temp->next->next );
							}
						}

						if ( newvar ){
							frame_add_var( frame, newvar->data, setvar );

						} else {
							printf( "[%s] Definition missing arguments\n", __func__ );
							stack_trace( frame );
						}

					} else if ( temp->type == TYPE_PROCEDURE ){
						builtin = true;
						printf( "[%s] has procedure\n", __func__ );
					}

					if ( !builtin ){
						printf( "[%s] Got here\n", __func__ );
						tempframe = frame_create( frame, NULL );

						/*
						for ( ; temp; temp = temp->next ){
							frame_add_token( tempframe, eval_tokens( tempframe, temp ));
						}
						*/
						frame_add_token( tempframe, eval_tokens( tempframe, temp ));

						eval_tokens( tempframe, tempframe->expr );
						stack_trace( tempframe );
					}

				} else {
					printf( "Error: empty expression\n" );
					stack_trace( frame );
					ret = NULL;
					builtin = true;
				}
			}
		}
	}

	return ret;
}

st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ret_pos ){
	st_frame_t *ret;

	ret = calloc( 1, sizeof( st_frame_t ));
	ret->last = cur_frame;
	ret->ret = ret_pos;

	ret->value = NULL;
	ret->expr = NULL;

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
	variable_t *new_var = NULL;

	if ( frame ){
		if ( !frame->vars )
			frame->vars = list_create( 0 );

		new_var = calloc( 1, sizeof( variable_t ));
		new_var->key = strdup( key );
		new_var->token = token;

		list_add_data( frame->vars, new_var );

	} else {
		printf( "[%s] Warning: Got null frame, can't add variable \"%s\"\n",
				__func__, key );
	}

	return new_var;
}

void stack_trace( st_frame_t *frame ){
	st_frame_t *move = frame;
	token_t *token = NULL;
	token_t *func = NULL;
	unsigned i;

	printf( "[stack trace]\n" );
	for ( move = frame, i = 0; move; move = move->last, i++ ){
		printf( "[%u] ", i );

		func = move->expr;
		token = func;

		foreach_in_list( token ){
			printf( "%s", type_str( token->type ));

			if ( token->type == TYPE_SYMBOL ){
				char *name = token->data;

				printf( " \"%s\"", name );
				printf( " (at %p)", frame_find_var( frame, name ));
			}

			if ( token->next )
				printf( " -> " );
		}

		printf( "\n" );
	}
}

token_t *frame_add_token( st_frame_t *frame, token_t *token ){
	token_t *ret = token;

	if ( !frame->expr ){
		frame->expr = frame->end = clone_token_tree( token );
		
	} else {
		frame->end->next = clone_token_tree( token );
		frame->end = frame->end->next;
	}

	frame->ntokens++;

	return ret;
}
