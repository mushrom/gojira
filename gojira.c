#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gojira/lexer.h>

void print_help( ){
	printf( "Usage: gojira [-f filename] [-hi]\n" );
}

token_t *dump_tokens( token_t *tokens, int level ){
	if ( tokens ){
		int i;
		for ( i = 0; i < level * 4; i++ )
			putchar( ' ' );

		printf( "Token, type: %d\n", tokens->type );
		dump_tokens( tokens->down, level + 1 );
		dump_tokens( tokens->next, level );
	}

	return tokens;
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
	
	// Debugging output
	printf( "> " );
	char buf[128];
	fgets( buf, 128, stdin );
	
	dump_tokens( lexerize( buf ), 0 );

	if ( interactive ){
		// enter REPL
	}

	return ret;
} 
