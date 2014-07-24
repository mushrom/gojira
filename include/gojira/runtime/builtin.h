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

token_t *builtin_equal( stack_frame_t *frame );
token_t *builtin_add( stack_frame_t *frame );
token_t *builtin_multiply( stack_frame_t *frame );
token_t *builtin_subtract( stack_frame_t *frame );
token_t *builtin_divide( stack_frame_t *frame );
token_t *builtin_display( stack_frame_t *frame );
token_t *builtin_newline( stack_frame_t *frame );
token_t *builtin_stacktrace( stack_frame_t *frame );

token_t *builtin_greaterthan( stack_frame_t *frame );
token_t *builtin_lessthan( stack_frame_t *frame );
token_t *builtin_car( stack_frame_t *frame );
token_t *builtin_cdr( stack_frame_t *frame );
token_t *builtin_is_null( stack_frame_t *frame );

// Functions for use by the interpreter's trampoline
token_t *builtin_return_last( stack_frame_t *frame );
token_t *builtin_return_first( stack_frame_t *frame );
token_t *builtin_intern_set( stack_frame_t *frame );

#endif
