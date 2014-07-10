#include <gojira/runtime/builtin.h>
#include <stdlib.h>
#include <stdio.h>

token_t *ext_proc_token( scheme_func handle ){
	token_t *ret = NULL;
	ext_proc_t *ext;

	ret = calloc( 1, sizeof( token_t ));
	ext = calloc( 1, sizeof( ext_proc_t ));

	ext->handler = handle;
	ret->data = ext;
	ret->type = TYPE_EXTERN_PROC;

	return ret;
}

token_t *builtin_define( stack_frame_t *frame ){
	token_t *ret = frame->expr;

	printf( "[%s] Got here\n", __func__ );
	if ( frame->ntokens != 3 )
		printf( "[%s] Have invalid number of tokens in define (have %d, expected 2)\n", __func__, frame->ntokens - 1 );

	ret = ret->next;

	return ret;
}

token_t *builtin_lambda( stack_frame_t *frame ){
	token_t *ret = frame->expr;

	printf( "[%s] Got here\n", __func__ );

	return ret;
}
