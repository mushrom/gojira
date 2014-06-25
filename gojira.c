#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>

void print_help( ){
	printf( "Usage: gojira [-f filename] [-hi]\n" );
}

int main( int argc, char *argv[] ){
	int ret = 0;
	char option;
	int i;
	bool interactive = false;
	FILE *input_file = NULL;

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

				default:
					print_help( );
					exit( 1 );
					break;
			}
		}
	}

	// parse_file( input_file );
	// initialize_runtime( flags );

	if ( interactive ){
		// enter REPL
	}

	return ret;
} 
