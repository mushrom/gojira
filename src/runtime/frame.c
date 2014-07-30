#include <gojira/runtime/frame.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void stack_trace( st_frame_t *frame ){
	st_frame_t *move = frame;
	token_t *token = NULL;
	list_node_t *vars;
	variable_t *var;
	unsigned i, k, limit = 5;
	int start = 0, m;

	printf( "[stack trace]\n" );
	for ( move = frame, i = 0; move; move = move->last, i++ ){
		printf( "[%u] ", i );

		token = move->expr;

		if ( move->vars )
			vars = move->vars->base;
		else
			vars = NULL;

		start = move->ntokens - limit;

		k = m = 0;
		foreach_in_list( token ){
			if ( m >= start || m == 0 ){
				printf( "%s", type_str( token->type ));

				if ( token->type == TYPE_SYMBOL ){
					char *name = token->data;

					printf( " \"%s\"", name );
					printf( " (at %p)", frame_find_var( frame, name ));
				}

				if ( token->next )
					printf( " -> " );

				k++;
				m++;

			} else {
				printf( ".. " );
				m++;
			}
		}

		printf( "\n" );

		if ( vars ){
			printf( " `- has variables: " );

			k = 0;
			foreach_in_list( vars ){
				var = vars->data;
				printf( "%-10s  ", var->key );

				k = (k + 1) % 8;
				if ( !k )
					printf( "\n                   " );
			}

			printf( "\n" );
		}
	}
}

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

st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr ){
	st_frame_t *ret;

	ret = calloc( 1, sizeof( st_frame_t ));
	ret->last = cur_frame;
	ret->ptr = ptr;

	ret->value = NULL;
	ret->expr = NULL;
	ret->status = TYPE_NULL;

	return ret;
}

st_frame_t *frame_free( st_frame_t *frame ){
	//printf( "[%s] Freeing frame\n", __func__ );
	list_node_t *move;
	variable_t *var;

	if ( frame ){
		if ( frame->vars ){
			move = frame->vars->base;
			foreach_in_list( move ){
				var = move->data;
				free_tokens( var->token );
				free( var );
			}

			list_free( frame->vars );
		}

		free_tokens( frame->expr );
		frame->expr = NULL;
		free( frame );
	}

	return NULL;
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
		new_var->token = clone_token_tree( token );

		list_add_data( frame->vars, new_var );

	} else {
		printf( "[%s] Warning: Got null frame, can't add variable \"%s\"\n",
				__func__, key );
	}

	return new_var;
}
