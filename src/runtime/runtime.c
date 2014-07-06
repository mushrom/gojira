#include <gojira/runtime/runtime.h>
#include <gojira/parse_debug.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

token_t *eval_tokens( continuation_t *cont, token_t *tokens ){
	token_t *ret = tokens;
	token_t expr;
	token_t *move;
	cont_t *temp_cont;

	memset( &expr, 0, sizeof( token_t ));
	move = &expr;

	switch ( tokens->type ){
		case TYPE_LIST:
			// Create continuation, evaluate tokens in list
			break;

		default:
			printf( "[%s] Don't know what to do with token of type \"%s\", "
					"bailing out now.\n", __func__, type_str( tokens->type ));
			break;
	}

	return ret;
}

cont_t *cont_create( cont_t *cur_cont, token_t *ret_pos ){
	cont_t *ret;

	ret = calloc( 1, sizeof( cont_t ));
	ret->last = cur_cont;
	ret->ret = ret_pos;

	return ret;
}
