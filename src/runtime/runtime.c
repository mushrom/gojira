#include <gojira/runtime/runtime.h>
#include <gojira/parse_debug.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

cont_t *eval_loop( cont_t *cont, token_t *tokens ){
	token_t expr;
	token_t *move;
	token_t *cptr;
	cont_t *cur_cont;

	memset( &expr, 0, sizeof( token_t ));
	move = &expr;
	cptr = tokens;

	cur_cont = cont;

	while ( cur_cont ){
		while ( cptr ){
			if ( cptr->type == TYPE_LIST && cptr->down ){
				// Create continuation, evaluate tokens in list
				printf( "[%s] Got a list, ret = %p\n", __func__, cptr );

				if ( cptr->down->type == TYPE_SYMBOL && (
						strcmp( cptr->down->data, "define" ) == 0 ||
						strcmp( cptr->down->data, "lambda" ) == 0 )){

					//handle special functions here

					cur_cont->value = cptr;
					cptr = cptr->next;

				} else {
					if ( cptr->down->type == TYPE_SYMBOL ){
						printf( "[%s] Will be evaluating \"%s\"...\n",
								__func__, cptr->down->data );
					}

					cur_cont = cont_create( cur_cont, cptr->next );
					cptr = cptr->down;
				}

			} else {
				printf( "[%s] Don't know what to do with token of type \"%s\", "
						"continuing...\n", __func__, type_str( cptr->type ));

				cur_cont->value = cptr;
				cptr = cptr->next;
			}
		}

		// return from current continuation, if cur_cont->ret
		// is not null
		printf( "[%s] Returning to %p\n", __func__, cur_cont->ret );
		cptr = cur_cont->ret;
		cur_cont = cur_cont->last;
	}

	return cont;
}

token_t *eval_tokens( continuation_t *cont, token_t *tokens ){
	token_t *ret = tokens;
	cont_t *temp_cont = cont_create( NULL, NULL );

	eval_loop( temp_cont, tokens );

	return ret;
}

cont_t *cont_create( cont_t *cur_cont, token_t *ret_pos ){
	cont_t *ret;

	ret = calloc( 1, sizeof( cont_t ));
	ret->last = cur_cont;
	ret->ret = ret_pos;

	return ret;
}

variable_t *cont_add_var( cont_t *cont, char *key, token_t *token ){
	variable_t *new_var;

	if ( !cont->vars )
		cont->vars = list_create( 0 );

	new_var = calloc( 1, sizeof( variable_t ));
	new_var->key = strdup( key );
	new_var->token = token;

	list_add_data( cont->vars, new_var );

	return new_var;
}
