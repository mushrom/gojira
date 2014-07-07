#include <gojira/runtime/runtime.h>
#include <gojira/parse_debug.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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
					cptr = cptr->down;
				}

			} else {
				printf( "[%s] Don't know what to do with token of type \"%s\", "
						"continuing...\n", __func__, type_str( cptr->type ));

				cur_frame->value = cptr;
				cptr = cptr->next;
			}
		}

		// return from current frameinuation, if cur_frame->ret
		// is not null
		printf( "[%s] Returning to %p\n", __func__, cur_frame->ret );
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
