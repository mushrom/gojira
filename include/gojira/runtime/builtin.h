#ifndef _GOJIRA_RUNTIME_BUILTIN_H
#define _GOJIRA_RUNTIME_BUILTIN_H
#ifdef __cplusplus
extern "C" {
#endif

#include <gojira/runtime/runtime.h>
#include <gojira/tokens.h>

typedef token_t *(*scheme_func)( stack_frame_t * );

typedef struct ext_proc {
	scheme_func handler;

	unsigned flags;
	unsigned references;
} ext_proc_t;

token_t *ext_proc_token( scheme_func handle );

token_t *builtin_equal( stack_frame_t *frame );
token_t *builtin_add( stack_frame_t *frame );
token_t *builtin_modulo( stack_frame_t *frame );
token_t *builtin_multiply( stack_frame_t *frame );
token_t *builtin_subtract( stack_frame_t *frame );
token_t *builtin_divide( stack_frame_t *frame );
token_t *builtin_display( stack_frame_t *frame );
token_t *builtin_newline( stack_frame_t *frame );
token_t *builtin_stacktrace( stack_frame_t *frame );
token_t *builtin_sleep( stack_frame_t *frame );

token_t *builtin_greaterthan( stack_frame_t *frame );
token_t *builtin_lessthan( stack_frame_t *frame );
token_t *builtin_car( stack_frame_t *frame );
token_t *builtin_cdr( stack_frame_t *frame );
token_t *builtin_is_null( stack_frame_t *frame );
token_t *builtin_is_list( stack_frame_t *frame );

// String functions
token_t *builtin_string_append( stack_frame_t *frame );
token_t *builtin_string_to_symbol( stack_frame_t *frame );
token_t *builtin_symbol_to_string( stack_frame_t *frame );
token_t *builtin_string_contains( stack_frame_t *frame );
token_t *builtin_char_to_string( stack_frame_t *frame );
token_t *builtin_is_string( stack_frame_t *frame );
token_t *builtin_string_ref( stack_frame_t *frame );

// boolean functions
token_t *builtin_true( stack_frame_t *frame );
token_t *builtin_false( stack_frame_t *frame );

// vector functions
token_t *builtin_is_vector( stack_frame_t *frame );
token_t *builtin_vector_ref( stack_frame_t *frame );
token_t *builtin_vector_set( stack_frame_t *frame );

// Functions for use by the interpreter's trampoline
token_t *builtin_return_last( stack_frame_t *frame );
token_t *builtin_return_first( stack_frame_t *frame );
token_t *builtin_intern_set( stack_frame_t *frame );
token_t *builtin_intern_set_global( stack_frame_t *frame );

token_t *builtin_read_char( stack_frame_t *frame );
token_t *builtin_cons( stack_frame_t *frame );
token_t *builtin_list( stack_frame_t *frame );
token_t *builtin_load_global_file( stack_frame_t *frame );

// File functions
token_t *builtin_open( stack_frame_t *frame );
token_t *builtin_readall( stack_frame_t *frame );
token_t *builtin_writechar( stack_frame_t *frame );

token_t *builtin_iterator_next( stack_frame_t *frame );
token_t *builtin_iterator_access( stack_frame_t *frame );
token_t *builtin_iterator( stack_frame_t *frame );

#ifdef __cplusplus
}
#endif
#endif
