#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gojira/lexer.h>
#include <gojira/parser.h>
#include <gojira/config.h>

void print_help( ){
	printf( "Usage: gojira [-f filename] [-hi]\n" );
}

int main( int argc, char *argv[] ){
	int ret = 0;
	char option;
	int i;
	bool interactive = false;
	FILE *input_file = NULL;

	initialize_config( );
	set_config_option( "testing", 1 );
	printf( "Config option testing: %li\n", get_config_option( "testing" ));

	if ( argc < 2 ){
		interactive = true;

	} else {
		while (( option = getopt( argc, argv, "f:hi" )) != -1 && i++ < argc ){
			switch ( option ){
				case 'f':
					printf( "Have file \"%s\"\n", argv[++i] );
					break;

				case 'i':
					interactive = true;
					break;

				case 'h':
					print_help( );
					exit( 0 );
					break;

				case 'v':
					set_config_option( "verbose", 1 );
					set_config_option( "verbose", 1 );
					break;

				default:
					print_help( );
					exit( 1 );
					break;
			}
		}
	}

	// initialize_runtime( flags );

	if ( interactive ){
		// enter REPL
		while ( 1 ){
			token_t *tree;
			// Debugging output, will be an actual REPL at some point.
			printf( "> " );
			char buf[128];
			fgets( buf, 128, stdin );

			tree = parse_tokens( dump_tokens( lexerize( buf ), 0 ));
			tree = remove_punc_tokens( tree );

			dump_tokens( tree, 0 );
		}
	}

	return ret;
} 
