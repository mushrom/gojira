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
#include <linenoise/linenoise.h>

// Some function definitions used only in this file.
void print_help( );
char *read_input_file( FILE *fp );
char *read_with_parens( FILE *fp );

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

	// Initialize the global interpreter state
	global_frame = frame_create( NULL, NULL );
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
		char *buf = "";

		while ( buf ){
			// Read a statement
			//printf( "> " );
			//char *buf = read_with_parens( stdin );
			buf = linenoise( "> " );

			if ( buf ){
				// generate a parse tree, and make sure all tokens are "clean" for the GC
				tree = remove_punc_tokens( parse_tokens( lexerize( buf )));
				gc_unmark( tree );

				// Only interpret the tree if there is a tree
				if ( tree ){
					linenoiseHistoryAdd( buf );
					global_frame->ptr = tree;

					eval_loop( global_frame, tree );
					print_token( global_frame->end );
					putchar( '\n' );

					free_tokens( tree );
				}
			}

			// Free the statement that was read
			free( buf );
		}
	}

	// Clean up the global frame, and free all tokens left in the token cache
	gc_sweep( global_frame->heap );
	frame_free( global_frame );
	destroy_token_cache( );

	return ret;
} 

// Displays the handy help message dialog
void print_help( ){
	printf( "Usage: gojira [-hi] [files]\n" );
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
