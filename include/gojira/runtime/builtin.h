#ifndef _GOJIRA_RUNTIME_BUILTIN_H
#define _GOJIRA_RUNTIME_BUILTIN_H 1
#ifdef __cplusplus
extern "C" {
#endif

#include <gojira/runtime/runtime.h>
#include <gojira/tokens.h>
#include <gojira/debugger/debugger.h>

#include <gojira/runtime/builtins/bytevector.h>

// Convenience macros for displaying various errors builtins may have
#define FRAME_ERROR( frame, msg, ... ) \
	frame->error_call( frame, "[%s] Error: " msg "\n", \
		__func__, __VA_ARGS__ );

#define FRAME_ERROR_ARGTYPE( frame, expected, actual ) \
	frame->error_call( frame, "[%s] Error: expected " #expected ", but got %s\n", \
		__func__, type_str( actual ));

#define FRAME_ERROR_ARGNUM( frame, expected ) \
	frame->error_call( frame, \
		"[%s] Error: Expected " #expected " arguments, but have %u\n", \
		__func__, frame->ntokens - 1 );

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
token_t *builtin_write( stack_frame_t *frame );
token_t *builtin_newline( stack_frame_t *frame );
token_t *builtin_stacktrace( stack_frame_t *frame );
token_t *builtin_sleep( stack_frame_t *frame );

token_t *builtin_greaterthan( stack_frame_t *frame );
token_t *builtin_lessthan( stack_frame_t *frame );
token_t *builtin_car( stack_frame_t *frame );
token_t *builtin_cdr( stack_frame_t *frame );
token_t *builtin_is_null( stack_frame_t *frame );
token_t *builtin_is_list( stack_frame_t *frame );
token_t *builtin_is_symbol( stack_frame_t *frame );

token_t *builtin_is_number( stack_frame_t *frame );
token_t *builtin_is_integer( stack_frame_t *frame );
token_t *builtin_is_rational( stack_frame_t *frame );
token_t *builtin_is_real( stack_frame_t *frame );

token_t *builtin_eval( stack_frame_t *frame );
token_t *builtin_apply( stack_frame_t *frame );

// miscellaneous
token_t *builtin_random_int( stack_frame_t *frame );

// String functions
token_t *builtin_string_append( stack_frame_t *frame );
token_t *builtin_string_to_symbol( stack_frame_t *frame );
token_t *builtin_symbol_to_string( stack_frame_t *frame );
token_t *builtin_string_contains( stack_frame_t *frame );
token_t *builtin_char_to_string( stack_frame_t *frame );
token_t *builtin_is_string( stack_frame_t *frame );
token_t *builtin_string_ref( stack_frame_t *frame );
token_t *builtin_string_length( stack_frame_t *frame );

// boolean functions
token_t *builtin_true( stack_frame_t *frame );
token_t *builtin_false( stack_frame_t *frame );

// vector functions
token_t *builtin_is_vector( stack_frame_t *frame );
token_t *builtin_vector_ref( stack_frame_t *frame );
token_t *builtin_vector_set( stack_frame_t *frame );
token_t *builtin_vector_length( stack_frame_t *frame );
token_t *builtin_make_vector( stack_frame_t *frame );
token_t *builtin_vector_from_list( stack_frame_t *frame );

// Functions for use by the interpreter's trampoline
token_t *builtin_return_last( stack_frame_t *frame );
token_t *builtin_return_first( stack_frame_t *frame );
token_t *builtin_intern_set( stack_frame_t *frame );
token_t *builtin_intern_set_global( stack_frame_t *frame );

token_t *builtin_cons( stack_frame_t *frame );
token_t *builtin_list( stack_frame_t *frame );
token_t *builtin_load_global_file( stack_frame_t *frame );

// File functions
token_t *builtin_open( stack_frame_t *frame );
token_t *builtin_readall( stack_frame_t *frame );
token_t *builtin_read_char( stack_frame_t *frame );
token_t *builtin_write_char( stack_frame_t *frame );
token_t *builtin_is_eof( stack_frame_t *frame );
token_t *builtin_read( stack_frame_t *frame );
token_t *builtin_mkdir( stack_frame_t *frame );
token_t *builtin_file_exists( stack_frame_t *frame );

// iterator functions
token_t *builtin_iterator_next( stack_frame_t *frame );
token_t *builtin_iterator_access( stack_frame_t *frame );
token_t *builtin_iterator( stack_frame_t *frame );

// networking functions
token_t *builtin_tcp_socket( stack_frame_t *frame );
token_t *builtin_tcp_getchar( stack_frame_t *frame );
token_t *builtin_tcp_putchar( stack_frame_t *frame );

// debugger breakpoint function
token_t *debugger_loop( stack_frame_t *frame );

// hashmap things
token_t *builtin_hashmap_get( stack_frame_t *frame );
token_t *builtin_hashmap_make( stack_frame_t *frame );

#ifdef __cplusplus
}
#endif
#endif
