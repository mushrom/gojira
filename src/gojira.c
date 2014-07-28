#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gojira/lexer.h>
#include <gojira/parser.h>
#include <gojira/config.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/libs/stack.h>

void print_help( ){
	printf( "Usage: gojira [-f filename] [-hi]\n" );
}

int main( int argc, char *argv[] ){
	int ret = 0;
	char option;
	int i = 0;
	bool interactive = false;
	char *fname = NULL;
	FILE *input_file = NULL;

	stack_frame_t *global_frame;
	token_t *tree;

	if ( argc < 2 ){
		interactive = true;

	} else {
		while (( option = getopt( argc, argv, "f:hi" )) != -1 && i++ < argc ){
			switch ( option ){
				case 'f':
					fname = argv[++i];
					interactive = false;
					printf( "Have file \"%s\"\n", fname );
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
		}
	}

	global_frame = frame_create( NULL, NULL );
	init_global_frame( global_frame );

	if ( fname ){
		unsigned rsize = 0x1000;
		char *buf;

		input_file = fopen( fname, "r" );

		if ( input_file ){
			buf = malloc( rsize );

			fread( buf, rsize, 1, input_file );
			tree = remove_punc_tokens( parse_tokens( lexerize( buf )));

			global_frame->ptr = tree;
			eval_loop( global_frame, tree );

			free_tokens( tree );
			free( buf );
		}
	}

	if ( interactive ){
		while ( 1 ){
			printf( "> " );
			char buf[128];
			fgets( buf, 128, stdin );

			if ( strlen( buf ) > 1){
				tree = remove_punc_tokens( parse_tokens( lexerize( buf )));
				global_frame->ptr = tree;

				eval_loop( global_frame, tree );
				dump_tokens( global_frame->end, 0 );

				free_tokens( tree );
			}
		}
	}

	frame_free( global_frame );

	return ret;
} 
