#include <gojira/runtime/runtime.h>
#include <gojira/parse_debug.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

token_t *eval_function( st_frame_t *frame, list_head_t *tokens ){
	list_node_t *temp;
	token_t *token = NULL;
	temp = tokens->base;

	printf( "[%s] Dumping token list: ", __func__ );

	foreach_in_list( temp ){
		token = temp->data;

		if ( token ){
			printf( "%s", type_str( token->type ));
			if ( token->type == TYPE_SYMBOL )
				printf( " \"%s\"", (char *)token->data );
			printf( " -> " );
		}
	}

	printf( "\n" );

	return token;
}

st_frame_t *eval_loop( st_frame_t *frame, token_t *tokens ){
	token_t expr;
	token_t *move;
	token_t *cptr;
	st_frame_t *cur_frame;

	memset( &expr, 0, sizeof( token_t ));
	move = &expr;
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

					cur_frame->value = cptr;
					cptr = cptr->next;

				} else {
					if ( cptr->down->type == TYPE_SYMBOL ){
						printf( "[%s] Will be evaluating \"%s\"...\n",
								__func__, cptr->down->data );
					}

					cur_frame = frame_create( cur_frame, cptr->next );
					//list_add_data( cur_frame->expr, cptr->down );
					cptr = cptr->down;
				}

			} else {
				printf( "[%s] Don't know what to do with token of type \"%s\", "
						"continuing...\n", __func__, type_str( cptr->type ));

				if ( cur_frame->last && cur_frame->last->expr ){
					list_add_data( cur_frame->last->expr, cptr );
					printf( "[%s] Adding token to current expression list of type \"%s\"\n", __func__, type_str( cptr->type ));
					if ( cptr->type == TYPE_SYMBOL )
						printf( "[%s]\t symbol: \"%s\"\n", __func__, (char *)cptr->data );
				}

				cur_frame->value = cptr;
				cptr = cptr->next;
			}
		}

		// Evaluate function
		eval_function( cur_frame, cur_frame->expr );

		// return from current frame, if cur_frame->ret
		// is not null
		printf( "[%s] Returning to %p\n", __func__, cur_frame->ret );

		if ( cur_frame->last && cur_frame->last->expr && cur_frame->value ){
			//list_add_data( cur_frame->last->expr, cur_frame->value );
			//printf( "[%s] Returning token to last expression type \"%s\"\n", __func__, type_str( cptr->type ));
		} else if ( !cur_frame->value ){
			printf( "[%s] Warning: Frame returned no value, something might be broken\n", __func__ );
		}

		cptr = cur_frame->ret;
		cur_frame = cur_frame->last;
	}

	return frame;
}

token_t *eval_tokens( stack_frame_t *st_frame, token_t *tokens ){
	token_t *ret = tokens;
	st_frame_t *temp_frame = frame_create( NULL, NULL );

	eval_loop( temp_frame, tokens );

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
