#include <gojira/runtime/frame.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/hashmap.h>
#include <gojira/libs/shared.h>
#include <gojira/config.h>

#include <gojira/runtime/printer.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

// array of all builtin functions added by default
struct global_builtin {
	char        *name;
	scheme_func  handle;
} global_builtins[] = {
	// core functions
	{ "+",               builtin_add },
	{ "*",               builtin_multiply },
	{ "modulo",          builtin_modulo },
	{ "-",               builtin_subtract },
	{ "/",               builtin_divide },
	{ "eq?",             builtin_equal },
	{ "<",               builtin_lessthan },
	{ ">",               builtin_greaterthan },
	//{ "begin",           builtin_return_last },
	{ "car",             builtin_car },
	{ "cdr",             builtin_cdr },
	{ "cons",            builtin_cons },
	{ "list",            builtin_list },
	{ "null?",           builtin_is_null },
	{ "boolean?",        builtin_is_boolean },
	{ "list?",           builtin_is_list },
	{ "symbol?",         builtin_is_symbol },
	{ "char?",           builtin_is_char },
	{ "intern-set",      builtin_intern_set },
	{ "intern-set!",     builtin_intern_set_global },
	{ "true",            builtin_true },
	{ "false",           builtin_false },

	{ "number?",         builtin_is_number },
	{ "integer?",        builtin_is_integer },
	{ "rational?",       builtin_is_rational },
	{ "real?",           builtin_is_real },
	{ "floor",           builtin_floor },
	{ "char->integer",   builtin_char_to_integer },
	{ "integer->char",   builtin_integer_to_char },

	{ "eval",            builtin_eval },
	{ "apply",           builtin_apply },
	{ "intern-get-continuation", builtin_get_last_continuation },

	// string functions
	{ "string-append",   builtin_string_append },
	{ "string-contains", builtin_string_contains },
	{ "string->symbol",  builtin_string_to_symbol },
	{ "symbol->string",  builtin_symbol_to_string },
	{ "list->string",    builtin_char_to_string },
	{ "string?",         builtin_is_string },
	{ "string-ref",      builtin_string_ref },
	{ "string-length",   builtin_string_length },

	// vector functions
	{ "vector?",         builtin_is_vector },
	{ "vector-ref",      builtin_vector_ref },
	{ "vector-set!",     builtin_vector_set },
	{ "vector-length",   builtin_vector_length },
	{ "make-vector",     builtin_make_vector },
	{ "list->vector",    builtin_vector_from_list },

	//hashmap functions
	{ "hashmap",         builtin_hashmap_make },
	{ "hashmap-get",     builtin_hashmap_get },

	// misc
	{ "random-int",      builtin_random_int },

	{ "make-bytevector", builtin_make_bytevector },
	{ "bytevector-u8-ref", builtin_bytevector_u8_ref },
	{ "bytevector-length", builtin_bytevector_length },
	{ "u8-list->bytevector", builtin_bytevector_from_u8s },

#if ! GOJIRA_PUBLIC_MODE
	{ "stacktrace",      builtin_stacktrace },
	{ "intern-sleep",    builtin_sleep },
	{ "system",          builtin_system },
	{ "exit",            builtin_exit },

	// debugger breakpoint
	{ "debug-break",     debugger_loop },
#endif

#if GOJIRA_ENABLE_FILES
	// file functions
	{ "open",            builtin_open },
	{ "close",           builtin_close },
	{ "readall",         builtin_readall },
	{ "read-char",       builtin_read_char },
	{ "write-char",      builtin_write_char },
	{ "load!",           builtin_load_global_file },
	{ "eof?",            builtin_is_eof },
	{ "display",         builtin_display },
	{ "write",           builtin_write },
	{ "newline",         builtin_newline },
	{ "read",            builtin_read },
	{ "mkdir",           builtin_mkdir },
	{ "exists?",         builtin_file_exists },
#endif

#if GOJIRA_ENABLE_SOCKETS
	// networking functions
	{ "tcp-socket",      builtin_tcp_socket },
	{ "tcp-getchar",     builtin_tcp_getchar },
	{ "tcp-putchar",     builtin_tcp_putchar },
#endif
};

// Adds an "external function" to a frame, and handles registering the tokens for garbage collection
variable_t *global_add_func( stack_frame_t *frame, char *name, scheme_func handle ){
	token_t *proc = gc_register( get_current_gc( frame ), ext_proc_token( handle ));
	variable_t *ret = env_add_var( frame->env, name, proc, NO_RECURSE, VAR_MUTABLE_BUILTIN );

	return ret;
}

st_frame_t *init_global_frame( st_frame_t *frame ){
	int i;

	for ( i = 0; i < sizeof( global_builtins ) / sizeof( struct global_builtin ); i++ )
		global_add_func( frame, global_builtins[i].name, global_builtins[i].handle );

	return frame;
}

// TODO: move this to a more appropriate location
int utf8len( char *str ){
	int ret = 0;
	unsigned i;
	bool in_utf = false;

	for ( i = 0; str[i]; i++ ){
		if ( str[i] & 0x80 ){
			if ( !in_utf ){
				in_utf = true;
				ret++;
			}

		} else {
			in_utf = false;
			ret++;
		}
	}

	return ret;
}

// This gets the variable name corresponding to some global external procedure, if any.
// Returns NULL if not found.
static const char *stack_trace_ext_name( token_t *token ){
	const char *ret = NULL;
	int i;

	if ( token->type == TYPE_EXTERN_PROC ){
		for ( i = 0; i < sizeof( global_builtins ) / sizeof( struct global_builtin ); i++ ){
			if ( global_builtins[i].handle == token->func ){
				ret = global_builtins[i].name;
				break;
			}
		}
	}

	return ret;
}

void stack_trace( st_frame_t *frame ){
	st_frame_t *move = frame;
	token_t *token = NULL;

	unsigned i, k, limit = 3;
	int start = 0, m;
	const char *name;

	printf( "Stack trace:\n" );
	for ( move = frame, i = 0; move->last; move = move->last, i++ ){
		printf( "  %u: (", i );
		token = move->expr;
		start = move->ntokens - limit;

		k = m = 0;
		foreach_in_list( token ){
			if ( m >= start || m == 0 ){
				if (( name = stack_trace_ext_name( token ))){
					printf( "%s", name );
				} else {
					print_token( token, true );
				}

				if ( token->next )
					printf( " " );

				k++;
				m++;

			} else if ( m == 1 ){
				printf( "(%d truncated) ", start - 1);
				m++;

			} else {
				m++;
			}
		}

		if ( i != 0 && move->ntokens ){
			printf(" ...");
		}

		printf( ")\n" );
	}
}

void default_error_printer( stack_frame_t *frame, char *fmt, ... ){
	va_list args;
	va_start( args, fmt );

#if ! GOJIRA_PUBLIC_MODE
	stack_trace( frame );
#endif
	vprintf( fmt, args );

	va_end( args );
}

st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr, bool make_env ){
	st_frame_t *ret;

	ret = alloc_block( );
	ret->last = cur_frame;
	ret->ptr = ptr;

	ret->value = NULL;
	ret->expr = NULL;
	ret->status = TYPE_NULL;
	ret->flags  = RUNTIME_FLAG_NULL;
	ret->gc_link.type = GC_TYPE_CONTINUATION;

	if ( cur_frame ){
		ret->error_call = cur_frame->error_call;
		ret->flags |= cur_frame->flags & RUNTIME_FLAG_TRACE;
		ret->garbage = cur_frame->garbage;

		if ( !make_env && cur_frame->env ){
			ret->env = cur_frame->env;
		}

		gc_register( get_current_gc( ret ), ret );

	} else {
		ret->error_call = default_error_printer;
		ret->garbage = calloc( 1, sizeof( gbg_collector_t ));
		gc_init( NULL, ret->garbage );
	}

	// TODO: what happens when the global frame is created with no environment?
	if ( make_env ){
		ret->env = env_create( get_current_gc( ret ), cur_frame? cur_frame->env : NULL );
	}

	return ret;
}

// TODO: remove this at some point
st_frame_t *frame_free( st_frame_t *frame ){
	free( frame );

	return NULL;
}

token_t *frame_add_token( st_frame_t *frame, token_t *token ){
	token_t *ret = token;

	if ( !frame->expr ){
		frame->expr = frame->end = gc_clone_token( get_current_gc( frame ), token );
		frame->end->next = NULL;

	} else {
		frame->end->next = gc_clone_token( get_current_gc( frame ), token );
		frame->end = frame->end->next;
		frame->end->next = NULL;
	}

	frame->ntokens++;

	return ret;
}

token_t *frame_add_token_noclone( st_frame_t *frame, token_t *token ){
	token_t *ret = token;

	if ( !frame->expr ){
		frame->expr = frame->end = token;

	} else {
		frame->end->next = token;
		frame->end = frame->end->next;
	}

	frame->ntokens++;

	return ret;
}

stack_frame_t *frame_capture( stack_frame_t *frame ){
	stack_frame_t *temp = frame;

	for ( ; temp && !(temp->flags & RUNTIME_FLAG_CAPTURED); temp = temp->last ){
		temp->flags |= RUNTIME_FLAG_CAPTURED;
	}

	return frame;
}

stack_frame_t *frame_restore( stack_frame_t *frame ){
	stack_frame_t *ret = frame;

	if ( frame->last && frame->flags & RUNTIME_FLAG_CAPTURED ){
		ret = alloc_block_nozero( );
		*ret = *frame;
		gc_register( get_current_gc( ret ), ret );
		token_t *new_end = NULL;

		ret->expr = gc_clone_token_spine( get_current_gc( ret ), frame->expr );
		for ( new_end = ret->expr; new_end && new_end->next; new_end = new_end->next );

		ret->end = new_end;
		ret->flags &= ~RUNTIME_FLAG_CAPTURED;
	}

	return ret;
}
