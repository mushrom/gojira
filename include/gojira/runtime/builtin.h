#ifndef _GOJIRA_RUNTIME_BUILTIN_H
#define _GOJIRA_RUNTIME_BUILTIN_H
#include <gojira/runtime/runtime.h>
#include <gojira/tokens.h>

typedef token_t *(*scheme_func)( stack_frame_t * );

typedef struct ext_proc {
	scheme_func handler;

	unsigned flags;
} ext_proc_t;

token_t *ext_proc_token( scheme_func handle );

token_t *builtin_add( stack_frame_t *frame );
token_t *builtin_multiply( stack_frame_t *frame );
token_t *builtin_subtract( stack_frame_t *frame );
token_t *builtin_divide( stack_frame_t *frame );

#endif
