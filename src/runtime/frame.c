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
	{ "stacktrace",      builtin_stacktrace },
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
	{ "intern-sleep",    builtin_sleep },
	{ "true",            builtin_true },
	{ "false",           builtin_false },

	{ "eval",            builtin_eval },
	{ "apply",           builtin_apply },

	// debugger breakpoint
	{ "debug-break",     debugger_loop },

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

#if GOJIRA_ENABLE_FILES
	// file functions
	{ "open",            builtin_open },
	{ "readall",         builtin_readall },
	{ "read-char",       builtin_read_char },
	{ "write-char",      builtin_write_char },
	{ "load!",           builtin_load_global_file },
	{ "eof?",            builtin_is_eof },
	{ "display",         builtin_display },
	{ "newline",         builtin_newline },
	{ "read",            builtin_read },
	{ "mkdir",           builtin_mkdir },
	{ "exists?",         builtin_file_exists },
#endif

	// iterator functions
	{ "iterator",        builtin_iterator },
	{ "iter-car",        builtin_iterator_access },
	{ "iter-cdr",        builtin_iterator_next },

#if GOJIRA_ENABLE_SOCKETS
	// networking functions
	{ "tcp-socket",      builtin_tcp_socket },
	{ "tcp-getchar",     builtin_tcp_getchar },
	{ "tcp-putchar",     builtin_tcp_putchar },
#endif

	//hashmap functions
	{ "hashmap",         builtin_hashmap_make },
	{ "hashmap-get",     builtin_hashmap_get },
};

// Adds an "external function" to a frame, and handles registering the tokens for garbage collection
variable_t *global_add_func( st_frame_t *frame, char *name, scheme_func handle ){
	return frame_add_var( frame, name,
		frame_register_one_token( frame, ext_proc_token( handle )),
			NO_RECURSE, VAR_IMMUTABLE );
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
				print_token( token );

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
	}
}

void default_error_printer( stack_frame_t *frame, char *fmt, ... ){
	va_list args;
	va_start( args, fmt );

	stack_trace( frame );
	vprintf( fmt, args );
	//printf( "[%s] got here\n", __func__ );

	va_end( args );
}

st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr ){
	st_frame_t *ret;

	ret = calloc( 1, sizeof( st_frame_t ));
	ret->last = cur_frame;
	ret->ptr = ptr;

	ret->value = NULL;
	ret->expr = NULL;
	ret->status = TYPE_NULL;
	ret->flags  = RUNTIME_FLAG_NULL;

	if ( cur_frame ){
		ret->error_call = cur_frame->error_call;
		ret->flags |= cur_frame->flags & RUNTIME_FLAG_TRACE;

	} else {
		ret->error_call = default_error_printer;
	}

	return ret;
}

st_frame_t *frame_free( st_frame_t *frame ){
	list_node_t *move, *temp;
	hashmap_t *map;
	unsigned i;

	if ( frame ){
		if ( frame->vars ){
			map = frame->vars;
			for ( i = 0; i < map->nbuckets; i++ ){
				move = map->buckets[i].base;

				for ( ; move; move = temp ){
					temp = move->next;
					shared_release( move->data );
					free( move );
				}
			}

			hashmap_free( frame->vars );
		}

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

variable_t *frame_find_var_struct_hash( st_frame_t *frame, unsigned hash, bool recurse ){
	variable_t *ret = NULL;
	shared_t *shr;

	if ( frame ){
		if ( frame->vars ){
			shr = hashmap_get( frame->vars, hash );
			if ( shr ){
				ret = shared_get( shr );
			}

			if ( !ret && recurse ){
				ret = frame_find_var_struct_hash( frame->last, hash, recurse );
			}

		} else if ( recurse ){
			ret = frame_find_var_struct_hash( frame->last, hash, recurse );
		}
	}

	return ret;
}

shared_t *frame_find_shared_struct_hash( st_frame_t *frame, unsigned hash, bool recurse ){
	shared_t *ret = NULL;

	if ( frame ){
		if ( frame->vars ){
			ret = hashmap_get( frame->vars, hash );

			if ( !ret && recurse ){
				ret = frame_find_shared_struct_hash( frame->last, hash, recurse );
			}

		} else if ( recurse ){
			ret = frame_find_shared_struct_hash( frame->last, hash, recurse );
		}
	}

	return ret;
}

token_t *frame_find_var_hash( st_frame_t *frame, unsigned hash, bool recurse ){
	token_t *ret = NULL;
	variable_t *var;

	var = frame_find_var_struct_hash( frame, hash, recurse );
	if ( var ){
		ret = var->token;
	}

	return ret;
}

token_t *frame_find_var( st_frame_t *frame, char *key, bool recurse ){
	token_t *ret = NULL;
	unsigned hash;

	hash = hash_string( key );
	ret = frame_find_var_hash( frame, hash, recurse );

	return ret;
}

variable_t *frame_find_var_struct( st_frame_t *frame, char *key, bool recurse ){
	variable_t *ret = NULL;
	unsigned hash;

	hash = hash_string( key );
	ret = frame_find_var_struct_hash( frame, hash, recurse );

	return ret;
}

shared_t *frame_find_shared_struct( st_frame_t *frame, char *key, bool recurse ){
	shared_t *ret = NULL;
	unsigned hash;

	hash = hash_string( key );
	ret = frame_find_shared_struct_hash( frame, hash, recurse );

	return ret;
}

void free_var( void *ptr ){
	if ( ptr ){
		variable_t *var = ptr;
		//printf( "[%s] Freeing variable with hash 0x%x\n", __func__, var->hash );
		free_tokens( var->token );
		free( var->key );
		free( var );
	}
}

variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token, bool recurse, bool mutable ){
	variable_t *new_var = NULL;
	shared_t *new_shared = NULL;

	if ( frame ){
		if ( !frame->vars )
			frame->vars = hashmap_create( 8 );

		new_var = frame_find_var_struct( frame, key, recurse );

		if ( !new_var ){
			new_var = calloc( 1, sizeof( variable_t ));
			new_var->key = strdup( key );
			new_var->hash = hash_string( key );
			new_var->is_mutable = mutable;
			new_var->token = clone_token_tree( token );
			new_shared = shared_new( new_var, free_var );

			hashmap_add( frame->vars, new_var->hash, new_shared );

		} else if ( new_var->is_mutable ){
			new_var->token = clone_token_tree( token );

		} else {
			/* TODO: existing variable isn't mutable, so error out */
			printf( "[%s] Error: variable \"%s\" is not mutable\n",
					__func__, key );

			stack_trace( frame );
		}

	} else {
		printf( "[%s] Warning: Got null frame, can't add variable \"%s\"\n",
				__func__, key );
	}

	return new_var;
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
