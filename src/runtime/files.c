#include <gojira/lexer.h>
#include <gojira/parser.h>
#include <gojira/config.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/files.h>

#include <stdlib.h>

// Allocates a buffer, reads the entire file into it, and returns the buffer
char *read_input_file( FILE *fp ){
	char *ret = NULL;
	long size;

	fseek( fp, 0, SEEK_END );
	size = ftell( fp );
	fseek( fp, 0, SEEK_SET );

	ret = malloc( sizeof( char[ size + 1 ]));
	fread( ret, 1, size, fp );
	ret[size] = 0;

	return ret;
}

bool evaluate_file( stack_frame_t *frame, char *filename ){
    FILE *fp = fopen( filename, "r" );
    bool ret = false;
    char *buf;
    token_t *foo;
	token_t *(*token_parser)(char *) = parse_scheme_tokens;

    if ( fp ){
        buf = read_input_file( fp );

        if ( buf ){
			char *type = strchr( filename, '.' );

			//foo = lexerize( buf );

			if ( type && (strcmp(type, ".mlisp") == 0 )){
				//printf( "[%s] Got here, would use alternate parser\n", __func__ );
				//foo = preprocess_mlisp( foo );
				token_parser = parse_mlisp_tokens;
			}

			//foo = remove_punc_tokens( parse_tokens( remove_meta_tokens( foo )));
            foo = token_parser( buf );
            //gc_unmark( foo );

            if ( foo ){
                frame->ptr = foo;
				//gc_register_tokens( &frame->gc, foo );
                eval_loop( frame );
				//gc_collect( &frame->gc, NULL, 0 );
                //free_tokens( foo );
            }

            ret = true;
            free( buf );
        }

        fclose( fp );

    } else {
        perror( filename );
    }

    return ret;
}
