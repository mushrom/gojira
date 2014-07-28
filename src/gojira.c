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

char *read_input_file( FILE *fp );
char *read_with_parens( FILE *fp );

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
		char *buf;

		if ( interactive )
			printf( "Have file \"%s\"\n", fname );

		input_file = fopen( fname, "r" );
		if ( input_file ){
			buf = read_input_file( input_file );
			fclose( input_file );

			tree = remove_punc_tokens( parse_tokens( lexerize( buf )));

			global_frame->ptr = tree;
			eval_loop( global_frame, tree );

			free_tokens( tree );
			free( buf );

		} else {
			perror( fname );
		}
	}

	if ( interactive ){
		while ( 1 ){
			printf( "> " );
			char *buf = read_with_parens( stdin );

			tree = remove_punc_tokens( parse_tokens( lexerize( buf )));

			if ( tree ){
				global_frame->ptr = tree;

				eval_loop( global_frame, tree );
				dump_tokens( global_frame->end, 0 );

				free_tokens( tree );
			}

			free( buf );
		}
	}

	frame_free( global_frame );

	return ret;
} 

char *read_input_file( FILE *fp ){
	char *ret = NULL;
	long size;

	fseek( fp, 0, SEEK_END );
	size = ftell( fp );
	fseek( fp, 0, SEEK_SET );

	ret = malloc( sizeof( char[ size + 1 ]));
	fread( ret, 1, size, fp );

	return ret;
}


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

