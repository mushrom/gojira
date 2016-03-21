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
void print_help( );
char *read_input_file( FILE *fp );
char *read_with_parens( FILE *fp );

void goj_linenoise_complete( const char *buf, linenoiseCompletions *lc );

// needed because linenoise doesn't support passing some sort of datastructure
// to the completion callback, sorry
stack_frame_t *really_global_frame = NULL;

int main( int argc, char *argv[] ){
	int ret = 0;
	signed char option;
	int i = 0;
	int lastopt = 0;
	bool interactive = false;

	char *fname = NULL;

	stack_frame_t *global_frame;
	token_t *tree;

	if ( argc < 2 ){
		// By default, go into an REPL
		interactive = true;

	} else {
		// otherwise parse options
		lastopt = 1;

		while (( option = getopt( argc, argv, "hi" )) != -1 && i++ < argc ){
			switch ( option ){
				case 'f':
					fname = argv[++i];
					interactive = false;
					break;

				case 'i':
					interactive = true;
					break;

				case 'h':
					print_help( );
					exit( 0 );
					break;

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

	// Load the 'base' library for needed primatives
	evaluate_file( global_frame, BASE_LIB );

	// If there were files passed, interpet them
	if ( lastopt ){
		for ( i = lastopt; i < argc; i++ ){
			fname = argv[i];

			// Let the user know what files are being interpreted, if in an REPL
			if ( interactive )
				printf( "Have file \"%s\"\n", fname );

            evaluate_file( global_frame, fname );
		}
	}

	// Go into the REPL if the interpreter flag is set
	if ( interactive ){
		linenoiseSetMultiLine( 1 );
		linenoiseSetCompletionCallback( goj_linenoise_complete );
		really_global_frame = global_frame;
		char *buf = "";
		unsigned n = 0;

		while ( buf ){
			// Read a statement
			//printf( "> " );
			//char *buf = read_with_parens( stdin );
			buf = linenoise( "> " );
			n++;

			if ( buf ){
				// generate a parse tree, and make sure all tokens are "clean" for the GC
				//tree = remove_punc_tokens( parse_tokens( remove_meta_tokens( lexerize( buf ))));
				tree = parse_scheme_tokens( buf );
				//gc_unmark( tree );

				// Only interpret the tree if there is a tree
				if ( tree ){
					char varexpr[64];
					linenoiseHistoryAdd( buf );
					global_frame->ptr = tree;

					eval_loop( global_frame );

					snprintf( varexpr, sizeof(varexpr) - 1, "..%u", n );

					// do clone here
					env_add_var( global_frame->env, varexpr,
						global_frame->end, NO_RECURSE, VAR_IMMUTABLE );

					env_add_var( global_frame->env, "..last",
						global_frame->end, NO_RECURSE, VAR_MUTABLE );

					printf( "..%u = ", n );
					print_token( global_frame->end, OUTPUT_READABLE );
					putchar( '\n' );

					//free_tokens( tree );
				}
			}

			// Free the statement that was read
			free( buf );
		}
	}

	// Clean up the global frame, and free all tokens left in the token cache
	//gc_sweep( global_frame->heap );
	gc_collect( &global_frame->gc, NULL, 0 );
	frame_free( global_frame );
	destroy_token_cache( );

	return ret;
} 

// Displays the handy help message dialog
void print_help( ){
	printf( "Usage: gojira [-hi] [files]\n" );
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
