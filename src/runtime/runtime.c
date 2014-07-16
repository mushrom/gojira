#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parser.h>
#include <gojira/lexer.h>
#include <gojira/parse_debug.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

st_frame_t *init_global_frame( st_frame_t *frame ){
	frame_add_var( frame, "+", ext_proc_token( builtin_add ));
	frame_add_var( frame, "*", ext_proc_token( builtin_multiply ));
	frame_add_var( frame, "-", ext_proc_token( builtin_subtract ));
	frame_add_var( frame, "/", ext_proc_token( builtin_divide ));
	frame_add_var( frame, "display", ext_proc_token( builtin_display ));
	frame_add_var( frame, "newline", ext_proc_token( builtin_newline ));
	frame_add_var( frame, "stacktrace", ext_proc_token( builtin_stacktrace ));
	frame_add_var( frame, "eq?", ext_proc_token( builtin_equal ));
	frame_add_var( frame, "<", ext_proc_token( builtin_lessthan ));
	frame_add_var( frame, ">", ext_proc_token( builtin_greaterthan ));

	return frame;
}

token_t *eval_all_tokens( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = tokens;
	token_t *move;

	for ( move = tokens; move; move = move->next ){
		ret = eval_tokens( frame, move );
	}

	return ret;
}

token_t *expand_procedure( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = tokens;
	token_t *move;
	token_t *temp;

	token_t *args;
	token_t *body;
	char *var_name;

	stack_frame_t *tempframe;

	if ( tokens->type == TYPE_PROCEDURE ){
		move = tokens->down;

		args = move->next;

		if ( args && args->type == TYPE_LIST ){
			body = args->next;
			temp = args->down;

			tempframe = frame_create( frame, NULL );
			move = tokens->next;
			
			foreach_in_list( temp ){
				if ( temp->type == TYPE_SYMBOL ){
					var_name = temp->data;

					/*
					printf( "[%s] procedure takes variable \"%s\"\n",
							__func__, var_name );
					*/

					if ( !move ){
						printf( "[%s] Error: Have unbound variable \"%s\"\n", __func__, var_name );
						break;
					}

					frame_add_var( tempframe, var_name, move );
					move = move->next;

				} else {
					printf( "[%s] Error: expected symbol in procedure definition\n", __func__ );
				}
			}

			ret = eval_all_tokens( tempframe, body );
		}

	} else {
		printf( "[%s] Error: Trying to apply non-procedure as procedure (?!)\n", __func__ );
	}

	return ret;
}

token_t *eval_function( st_frame_t *frame ){
	token_t *ret = frame->expr;
	token_t *move = ret;

	ext_proc_t *ext;
	scheme_func handle;

	switch ( move->type ){
		case TYPE_PROCEDURE:
			ret = expand_procedure( frame, move );
			break;

		case TYPE_EXTERN_PROC:
			//printf( "[%s] Have external function\n", __func__ );
			ext = move->data;
			handle = ext->handler;

			if ( handle )
				ret = handle( frame );

			break;

		default:
			printf( "[%s] Error: Unapplicable type \"%s\"\n", __func__, type_str( move->type ));
			stack_trace( frame );
			break;
	}

	return ret;
}

token_t *eval_tokens( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = tokens;
	token_t *temp;
	token_t *move;
	token_t *foo;
	char *name;
	st_frame_t *tempframe;

	//printf( "[%s] Got here\n", __func__ );
	
	if ( tokens ){
		move = tokens;
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
			temp = move->down;

			if ( move->down ){
				if ( temp->type == TYPE_DEFINE_EXPR ){

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

				} else if ( temp->type == TYPE_LAMBDA ){

					foo = calloc( 1, sizeof( token_t ));
					foo->type = TYPE_PROCEDURE;
					foo->down = temp;
					ret = foo;

					//printf( "[%s] has lambda, returning procedure\n", __func__ );

				} else if ( temp->type == TYPE_SYMBOL && ( strcmp( temp->data, "if" ) == 0 )){
						//printf( "[%s] Have \"if\" statement\n", __func__ );
						
						foo = eval_tokens( frame, temp->next );

						//printf( "[%s] if has \"%s\"\n", __func__, type_str( foo->type ));

						if ( foo->type == TYPE_BOOLEAN && foo->smalldata == false ){
							ret = clone_token_tree( eval_tokens( frame, temp->next->next->next ));
						} else {
							ret = clone_token_tree( eval_tokens( frame, temp->next->next ));
						}

				} else {
					tempframe = frame_create( frame, NULL );

					for ( ; temp; temp = temp->next ){
						frame_add_token( tempframe, eval_tokens( tempframe, temp ));
					}

					/*
					printf( "[%s] Evaluating procedure of type \"%s\"\n",
							__func__, type_str( tempframe->expr->type ));
					*/

					ret = eval_function( tempframe );
				}

			} else {
				printf( "Error: empty expression\n" );
				stack_trace( frame );
				ret = NULL;
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

			if ( !ret )
				ret = frame_find_var( frame->last, key );

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
	list_node_t *vars;
	variable_t *var;
	unsigned i;

	printf( "[stack trace]\n" );
	for ( move = frame, i = 0; move; move = move->last, i++ ){
		printf( "[%u] ", i );

		func = move->expr;
		token = func;
		if ( move->vars )
			vars = move->vars->base;
		else
			vars = NULL;

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

		if ( vars ){
			printf( " `- has variables " );
			foreach_in_list( vars ){
				var = vars->data;

				printf( "\"%s\" ", var->key );
			}

			printf( "\n" );
		}
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
