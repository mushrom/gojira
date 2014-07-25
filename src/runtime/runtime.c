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
	frame_add_var( frame, "car", ext_proc_token( builtin_car ));
	frame_add_var( frame, "cdr", ext_proc_token( builtin_cdr ));
	frame_add_var( frame, "null?", ext_proc_token( builtin_is_null ));

	frame_add_var( frame, "intern-set", ext_proc_token( builtin_intern_set ));

	return frame;
}

stack_frame_t *expand_procedure( stack_frame_t *frame, token_t *tokens ){
	stack_frame_t *ret = NULL;
	token_t *move;
	token_t *temp;

	token_t *args;
	token_t *body;
	char *var_name;

	if ( tokens->type == TYPE_PROCEDURE ){
		move = tokens->down;

		args = move->next;

		if ( args && args->type == TYPE_LIST ){
			body = args->next;
			temp = args->down;

			ret = frame_create( frame, body );
			frame_add_token( ret, ext_proc_token( builtin_return_last ));
			move = tokens->next;
			
			foreach_in_list( temp ){
				if ( temp->type == TYPE_SYMBOL ){
					var_name = temp->data;

					if ( !move ){
						printf( "[%s] Error: Have unbound variable \"%s\"\n", __func__, var_name );
						break;
					}

					frame_add_var( ret, var_name, move );
					move = move->next;

				} else {
					printf( "[%s] Error: expected symbol in procedure definition\n", __func__ );
				}
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

token_t *replace_symbol( token_t *tokens, token_t *replace, char *name ){
	token_t *ret = tokens;

	if ( tokens ){
		if ( tokens->type == TYPE_SYMBOL && ( strcmp( tokens->data, name )) == 0 ){
				ret = clone_token_tree( replace );
				ret->next = replace_symbol( tokens->next, replace, name );

		} else {
			ret->down = replace_symbol( ret->down, replace, name );
			ret->next = replace_symbol( ret->next, replace, name );
		}
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

	printf( "[%s] Expanding syntax rules\n", __func__ );

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
				printf( "[%s] Have pattern with same number of tokens\n", __func__ );
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

	dump_tokens( ret, 1 );

	return ret;
}

token_t *eval_loop( stack_frame_t *base, token_t *tokens ){
	stack_frame_t *frame = base;
	stack_frame_t *temp_frame;
	ext_proc_t *ext;
	token_t *ret = tokens;
	token_t *move;
	token_t *foo;
	scheme_func handle;

	bool running = true;
	bool have_error = false;

	while ( running && !have_error ){
		// Evaluate sub-expressions
		if ( frame->ptr ){
			switch ( frame->ptr->type ){
				case TYPE_LIST:
					move = frame->ptr;
					frame->ptr = frame->ptr->next;

					if ( move->down == NULL ){
						printf( "[%s] Error: Empty expression\n", __func__ );

						stack_trace( frame );
						have_error = true;
						break;
					}

					frame = frame_create( frame, move->down );
					break;

				case TYPE_SYMBOL:
					// search for symbol in the highest scope, which has the
					// most often used variables
					move = frame_find_var( base, frame->ptr->data );

					// didn't find it, start from top frame
					if ( !move )
						move = frame_find_var( frame, frame->ptr->data );

					if ( move ){
						frame_add_token( frame, move );

						if ( move->type != TYPE_SYNTAX ){
							frame->ptr = frame->ptr->next;

						} else {
							for ( frame->ptr = frame->ptr->next; frame->ptr; frame->ptr = frame->ptr->next )
								frame_add_token( frame, frame->ptr );
						}

					} else {
						printf( "[%s] Error: undefined variable \"%s\"\n",
								__func__, (char *)frame->ptr->data );

						stack_trace( frame );
						have_error = true;
					}

					break;

				case TYPE_LAMBDA:
					for ( ; frame->ptr; frame->ptr = frame->ptr->next )
						frame_add_token( frame, frame->ptr );

					break;

				case TYPE_DEF_SYNTAX:
					frame_add_token( frame, frame->ptr );
					frame_add_token( frame, frame->ptr->next );
					frame->ptr = frame->ptr->next->next;
					break;

				case TYPE_SYNTAX_RULES:
					move = calloc( 1, sizeof( token_t ));
					move->type = TYPE_SYNTAX;
					move->down = frame->ptr;

					frame_add_token( frame, ext_proc_token( builtin_return_first ));
					frame_add_token( frame, move );

					frame->ptr = NULL;
					break;

				case TYPE_SYNTAX:
					printf( "[%s] Expanding syntax macro...\n", __func__ );
					running = false;
					break;

				case TYPE_IF:
					move = expand_if_expr( frame, frame->ptr );

					if ( move ){
						frame_add_token( frame, move );
						frame->ptr = move->next;

					} else {
						have_error = true;
					}

					break;

				case TYPE_QUOTED_TOKEN:
					frame_add_token( frame, frame->ptr->down );
					frame->ptr = frame->ptr->next;
					break;

				default:
					frame_add_token( frame, frame->ptr );
					frame->ptr = frame->ptr->next;
					break;
			}

		// Evaluation finished, apply function
		} else {
			if ( frame->last ){
				//printf( "[%s] Applying type \"%s\"\n", __func__, type_str( frame->expr->type ));

				switch ( frame->expr->type ){
					case TYPE_EXTERN_PROC:
						ext = frame->expr->data;
						handle = ext->handler;

						if ( handle )
							frame->value = handle( frame );

						break;

					case TYPE_PROCEDURE:
						// TODO: Change this, either reuse the current frame or make 
						//       sure the frame is properly deallocated
						frame = expand_procedure( frame->last, frame->expr );
						continue;

					case TYPE_LAMBDA:
						foo = calloc( 1, sizeof( token_t ));
						foo->type = TYPE_PROCEDURE;
						foo->down = frame->expr;
						frame->value = foo;
						break;

					case TYPE_IF:
						if ( frame->expr->next->type == TYPE_BOOLEAN
								&& frame->expr->next->smalldata ){
							foo = clone_token_tree( frame->expr->down );

						} else {
							foo = clone_token_tree( frame->expr->down->next );
						}

						frame = frame_create( frame->last, foo );
						frame_add_token( frame, ext_proc_token( builtin_return_first ));

						continue;

					case TYPE_DEF_SYNTAX:
						printf( "[%s] Will be returning syntax definition\n", __func__ );
						frame_add_var( frame->last, frame->expr->next->data, frame->expr->next->next );

						frame->value = calloc( 1, sizeof( token_t ));
						frame->value->type = TYPE_NULL;

						break;

					case TYPE_SYNTAX:
						foo = expand_syntax_rules( frame, frame->expr );

						if ( foo ){
							if ( foo->type == TYPE_LIST ){
								frame->ptr = foo->down;
								frame->expr = NULL;
								continue;

							} else {
								frame->value = foo;
							}

							break;

						} else {
							have_error = true;
						}

						break;

					default:
						printf( "[%s] Can't apply \"%s\"\n", __func__, type_str( frame->expr->type ));
						stack_trace( frame );
						have_error = true;
						break;
				}

				if ( !have_error ){
					temp_frame = frame->last;
					frame_add_token( temp_frame, frame->value );

					frame = temp_frame;
				}

			} else {
				running = false;
			}
		}
	}

	ret = base->expr;

	return ret;
}

st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr ){
	st_frame_t *ret;

	ret = calloc( 1, sizeof( st_frame_t ));
	ret->last = cur_frame;
	ret->ptr = ptr;

	ret->value = NULL;
	ret->expr = NULL;

	return ret;
}

st_frame_t *frame_free( st_frame_t *frame ){
	//printf( "[%s] Freeing frame\n", __func__ );
	list_node_t *move;

	if ( frame ){
		if ( frame->vars ){
			move = frame->vars->base;
			foreach_in_list( move )
				free( move->data );

			list_free( frame->vars );
		}

		free_tokens( frame->expr );
		free( frame );
	}

	return NULL;
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
