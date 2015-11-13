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
	{ "begin",           builtin_return_last },
	{ "car",             builtin_car },
	{ "cdr",             builtin_cdr },
	{ "cons",            builtin_cons },
	{ "null?",           builtin_is_null },
	{ "list",            builtin_list },
	{ "list?",           builtin_is_list },
	{ "symbol?",         builtin_is_symbol },
	{ "intern-set",      builtin_intern_set },
	{ "intern-set!",     builtin_intern_set_global },
	{ "true",            builtin_true },
	{ "false",           builtin_false },

	{ "number?",         builtin_is_number },
	{ "integer?",        builtin_is_integer },
	{ "rational?",       builtin_is_rational },
	{ "real?",           builtin_is_real },
	{ "floor",           builtin_floor },

	{ "eval",            builtin_eval },
	{ "apply",           builtin_apply },

	// iterator functions
	{ "iterator",        builtin_iterator },
	{ "iter-car",        builtin_iterator_access },
	{ "iter-cdr",        builtin_iterator_next },

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

	// debugger breakpoint
	{ "debug-break",     debugger_loop },
#endif

#if GOJIRA_ENABLE_FILES
	// file functions
	{ "open",            builtin_open },
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
variable_t *global_add_func( env_t *env, char *name, scheme_func handle ){
	token_t *proc = ext_proc_token( handle );
	variable_t *ret = env_add_var( env, name, proc, NO_RECURSE, VAR_IMMUTABLE );

	free_token( proc );

	return ret;
}

st_frame_t *init_global_frame( st_frame_t *frame ){
	int i;

	for ( i = 0; i < sizeof( global_builtins ) / sizeof( struct global_builtin ); i++ )
		global_add_func( frame->env, global_builtins[i].name, global_builtins[i].handle );

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

void stack_trace( st_frame_t *frame ){
	st_frame_t *move = frame;
	token_t *token = NULL;
	list_node_t *vars;
	variable_t *var;
	hashmap_t *map;

	unsigned i, k, limit = 3;
	int start = 0, m;

	printf( "[stack trace]\n" );
	for ( move = frame, i = 0; move; move = move->last, i++ ){
		printf( "[%u] ", i );
		token = move->expr;
		start = move->ntokens - limit;

		k = m = 0;
		foreach_in_list( token ){
			if ( m >= start || m == 0 ){
				print_token( token, true );

				if ( token->next )
					printf( " -> " );

				k++;
				m++;

			} else if ( m == 1 ){
				printf( "(%d truncated) -> ", start - 1);
				m++;

			} else {
				//printf( ".. " );
				m++;
			}
		}

		printf( "\n" );
		/*
		map = move->vars;

		if ( map ){
			printf( " `- has variables:\n    " );
			k = 0;

			for ( m = 0; m < map->nbuckets; m++ ){
				vars = map->buckets[m].base;
				foreach_in_list( vars ){
					var = shared_get( vars->data );
					printf( "%s%*s", var->key, 16 - utf8len( var->key ), "" );

					k = (k + 1) % 6;
					if ( !k )
						printf( "\n    " );
				}
			}

			printf( "\n" );
		}
		*/
	}
}

void default_error_printer( stack_frame_t *frame, char *fmt, ... ){
	va_list args;
	va_start( args, fmt );

#if ! GOJIRA_PUBLIC_MODE
	stack_trace( frame );
#endif
	vprintf( fmt, args );
	//printf( "[%s] got here\n", __func__ );

	va_end( args );
}

st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr, bool make_env ){
	st_frame_t *ret;

	ret = calloc( 1, sizeof( st_frame_t ));
	ret->last = cur_frame;
	ret->ptr = ptr;

	ret->value = NULL;
	ret->expr = NULL;
	ret->status = TYPE_NULL;
	ret->flags  = RUNTIME_FLAG_NULL;

	// TODO: what happens when the global frame is created with no environment?
	if ( make_env ){
		ret->env = env_create( cur_frame? cur_frame->env : NULL );
	}

	if ( cur_frame ){
		ret->error_call = cur_frame->error_call;
		ret->flags |= cur_frame->flags & RUNTIME_FLAG_TRACE;

		if ( !make_env && cur_frame->env ){
			ret->env = env_aquire( cur_frame->env );
		}

	} else {
		ret->error_call = default_error_printer;
	}

	return ret;
}

st_frame_t *frame_free( st_frame_t *frame ){
	/*
	list_node_t *move, *temp;
	hashmap_t *map;
	unsigned i;
	*/

	if ( frame ){
		//frame_free_vars( frame );
		env_release( frame->env );
		free( frame );
	}

	return NULL;
}

token_t *frame_add_token( st_frame_t *frame, token_t *token ){
	token_t *ret = token;
	token_t *meh;

	if ( !frame->expr ){
		//frame->expr = frame->end = meh = clone_token_tree( token );
		//frame_register_token_tree( frame, meh );
		frame->expr = frame->end = meh = clone_token( token );
		frame_register_one_token( frame, meh );
		meh->status = GC_UNMARKED;

	} else {
		frame->end->next = clone_token_tree( token );
		frame->end->next = frame_register_token_tree( frame, frame->end->next );
		frame->end->next->status = GC_UNMARKED;
		frame->end = frame->end->next;
	}

	frame->ntokens++;

	return ret;
}

token_t *frame_add_token_noclone( st_frame_t *frame, token_t *token ){
	token_t *ret = token;

	if ( !frame->expr ){
		frame->expr = frame->end = token;
		//frame_register_token_tree( frame, token );
		frame_register_one_token( frame, token );
		token->status = GC_UNMARKED;

	} else {
		frame->end->next = token;
		frame->end->next = frame_register_token_tree( frame, frame->end->next );
		frame->end->next->status = GC_UNMARKED;
		frame->end = frame->end->next;
	}

	frame->ntokens++;

	return ret;
}

token_t *frame_register_tokens( st_frame_t *frame, token_t *token ){
	if ( token ){
		frame_register_tokens( frame, token->down );
		frame_register_tokens( frame, token->next );

		token->gc_link = frame->heap;
		frame->heap = token;
	}

	return token;
}

token_t *frame_register_token_tree( st_frame_t *frame, token_t *token ){
	if ( token ){
		frame_register_tokens( frame, token->down );

		token->gc_link = frame->heap;
		frame->heap = token;
	}

	return token;
}

token_t *frame_register_one_token( st_frame_t *frame, token_t *token ){
	if ( token ){
		token->gc_link = frame->heap;
		frame->heap = token;
	}

	return token;
}


token_t *frame_alloc_token( st_frame_t *frame ){
	return frame_register_one_token( frame, alloc_token( ));
}
