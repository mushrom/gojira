#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <getopt.h>
#include <gojira/lexer.h>
#include <gojira/parser.h>
#include <gojira/config.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/libs/stack.h>
#include <gojira/runtime/files.h>
#include <gojira/runtime/printer.h>
#include <linenoise/linenoise.h>

// Some function definitions used only in this file.
void make_argument_var( stack_frame_t *frame, int lastopt, int argc, char *argv[] );
void read_eval_print( stack_frame_t *frame );
void print_help( );

char *read_input_file( FILE *fp );
char *read_with_parens( FILE *fp );

void goj_linenoise_complete( const char *buf, linenoiseCompletions *lc );

// needed because linenoise doesn't support passing some sort of datastructure
// to the completion callback
stack_frame_t *really_global_frame = NULL;

int main( int argc, char *argv[] ){
	int ret = 0;
	signed char option;
	int i = 0;
	int lastopt = 0;
	bool interactive = false;
	bool load_libs = true;
	bool set_a_gc_profile = false;
	unsigned new_gc_profile = 0;

	char *fname = NULL;

	stack_frame_t *global_frame;
	token_t *tree;

	if ( argc < 2 ){
		// By default, go into an REPL
		interactive = true;

	} else {
		// otherwise parse options
		lastopt = 1;

		while (( option = getopt( argc, argv, "hiLg:" )) != -1 && i++ < argc ){
			switch ( option ){
				case 'i':
					interactive = true;
					break;

				case 'h':
					print_help( );
					exit( 0 );
					break;

				case 'L':
					load_libs = false;
					break;

				case 'g':
					{
						char *profile = argv[++i];

						set_a_gc_profile = true;

						if ( strcmp( profile, "fast" ) == 0 ){
							new_gc_profile = GC_PROFILE_FAST;

						} else if ( strcmp( profile, "balanced" ) == 0 ){
							new_gc_profile = GC_PROFILE_BALANCED;

						} else if ( strcmp( profile, "lowmem" ) == 0 ){
							new_gc_profile = GC_PROFILE_LOWMEM;

						} else {
							printf( "Unknown garbage collector profile \"%s\", "
							        "using \"balanced\" instead...\n", profile );

							new_gc_profile = GC_PROFILE_BALANCED;
						}
					}


				case 'v':
					break;

				default:
					print_help( );
					exit( 1 );
					break;
			}

			/* This keeps track of where the argument after the current argument is,
			   in order to find the filenames after option parsing is done.          */
			lastopt = i + 1;
		}
	}

	// print some info about the interpreter going into a REPL
	if ( interactive ){
		printf( "%s\n", GOJIRA_BUILD_NAME );
	}

	// Initialize the global interpreter state
	global_frame = frame_create( NULL, NULL, MAKE_ENV );
	init_global_frame( global_frame );

	if ( set_a_gc_profile ){
		gc_set_profile( get_current_gc( global_frame ), new_gc_profile );
	}

	// Load the 'base' library for needed primatives
	if ( load_libs ){
		evaluate_file( global_frame, BASE_LIB );
	}

	make_argument_var( global_frame, lastopt, argc, argv );

	if ( lastopt ){
		evaluate_file( global_frame, argv[lastopt] );
	}

	// Go into the REPL if the interpreter flag is set
	if ( interactive ){
		read_eval_print( global_frame );
	}

	// Clean up the global frame, and free all tokens left in the token cache
	gc_collect( get_current_gc( global_frame ));
	//frame_free( global_frame );
	destroy_token_cache( );

	return ret;
} 

// Displays the handy help message dialog
void print_help( ){
	printf( "Usage: gojira [-hiLG] [file] [program arguments ...]\n"
			"\t-i: enter REPL after [file] is executed. If no file is specified this is the default.\n"
			"\t-G: Specify a garbage collector profile to use, out of the following:\n"
			"\t      lowmem   : run the garbage collector frequently to reduce memory usage\n"
			"\t      balanced : a compromise between speed and memory usage (default)\n"
			"\t      fast     : run the garbage collector infrequently for better performance\n"

			"\t-L: Don't load the base library when initializing. This option is really only\n"
			"\t    useful for debugging the interpreter's internals.\n"
			"\t-h: show this help and exit\n"
			);
}

void make_argument_var( stack_frame_t *frame, int lastopt, int argc, char *argv[] ){
	int i;
	token_t *arglist = NULL;

	if ( lastopt ){
		token_t *move;
		token_t *temp;
		for ( i = lastopt + 1; i < argc; i++ ){
			temp = gc_alloc_token( get_current_gc( frame ));
			char *arg = argv[i];

			temp->type = TYPE_STRING;
			temp->flags |= T_FLAG_HAS_SHARED;
			temp->data = shared_new( strdup( arg ), free_string );

			if ( !arglist ){
				arglist = move = temp;

			} else {
				move->next = temp;
				move = temp;
			}
		}
	}

	token_t *argvar = gc_alloc_token( get_current_gc( frame ));
	argvar->type = TYPE_LIST;
	argvar->down = arglist;
	env_add_var( frame->env, "*arguments*", argvar, NO_RECURSE, VAR_MUTABLE_BUILTIN );
}

void read_eval_print( stack_frame_t *frame ){
	linenoiseSetMultiLine( 1 );
	linenoiseSetCompletionCallback( goj_linenoise_complete );
	really_global_frame = frame;
	char *buf = "";
	unsigned n = 0;
	token_t *tree;

	while ( buf ){
		buf = linenoise( "> " );
		n++;

		if ( buf ){
			tree = parse_scheme_tokens( buf );

			if ( tree ){
				char varexpr[64];

				linenoiseHistoryAdd( buf );
				frame->ptr = tree;
				eval_loop( frame );

				snprintf( varexpr, sizeof(varexpr) - 1, "..%u", n );

				env_add_var( frame->env, varexpr,
							 frame->end, NO_RECURSE, VAR_IMMUTABLE );

				env_add_var( frame->env, "..last",
							 frame->end, NO_RECURSE, VAR_MUTABLE );

				printf( "..%u = ", n );
				print_token( frame->end, OUTPUT_READABLE );
				putchar( '\n' );
			}
		}

		// Free the statement that was read
		free( buf );
	}
}

void goj_linenoise_complete( const char *buf, linenoiseCompletions *lc ){
	const char        *pos, *move;
	const st_frame_t  *frame = really_global_frame;
	const hashmap_t   *map = frame->env->vars;
	const list_node_t *node;
	const variable_t  *var;
	unsigned i, k, pos_num;
	bool has_punct = false;

	if ( map ){
		for ( pos = move = buf, pos_num = k = 0; *move; move++, k++ ){
			if ( strchr( " ()[]{}\t\n", *move )) {
				pos = move + 1;
				pos_num = k;
				has_punct = true;
			}
		}

		for ( i = 0; i < map->nbuckets; i++ ){
			node = map->buckets[i].base;
			foreach_in_list( node ){
				var = shared_get( node->data );
				if ( strstr( var->key, pos )){
					char *newbuf = malloc( sizeof( char[ strlen(buf) + strlen(var->key) + 4 ]));
					strcpy( newbuf, buf );
					strncpy( newbuf + pos_num + has_punct, var->key, strlen(var->key) + 1 );
					linenoiseAddCompletion( lc, newbuf );
					free( newbuf );
				}
			}
		}
	}

	//linenoiseAddCompletion( lc, "foo" );
}

// Allocates a buffer, reads a complete (meaning with matching parenthesis) statement into it,
// and returns the buffer.
// Currently unused, leaving here in case it's useful in the future.
char *read_with_parens( FILE *fp ){
	unsigned pos, alloced, open;
	char *ret = NULL;
	char c = 0;

	open = alloced = pos = 0;

	for ( ; open || c != '\n'; pos++ ){
		if ( pos + 1 >= alloced ){
			alloced += 16;
			ret = realloc( ret, sizeof( char[ alloced + 1]));
		}
		
		c = fgetc( fp );

		if      ( c == '(' ) open++;
		else if ( c == ')' ) open--;
		else if ( c == '\n' && open ) printf( "| " );

		ret[pos] = c;
	}

	ret[pos] = 0;

	return ret;
}
