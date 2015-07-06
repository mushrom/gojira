#include <gojira/config.h>
#include <string.h>
#include <errno.h>

#if GOJIRA_ENABLE_FILES
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/printer.h>
#include <gojira/runtime/files.h>
#include <gojira/parse_debug.h>

// needed because fclose() technically doesn't match the type specification
// of shared_dtor, which requres a void return
static void free_file( void *ptr ){
	if ( ptr ){
		fclose( (FILE *)ptr );
		printf( "got here\n" );
	}
}

token_t *builtin_open( stack_frame_t *frame ){
	token_t *ret = NULL;
	char *fname;
	char *mode;
	FILE *fp;

	if ( frame->ntokens >= 3 ){
		if ( frame->expr->next->type == TYPE_STRING ){
			fname = shared_get( frame->expr->next->data );

			if ( frame->expr->next->next->type == TYPE_STRING ){
				mode = shared_get( frame->expr->next->next->data );

				if (( fp = fopen( fname, mode ))){
					ret = alloc_token( );
					ret->type  = TYPE_FILE;
					ret->flags = T_FLAG_HAS_SHARED;
					ret->data  = shared_new( fp, free_file );

				} else {
					frame->error_call( frame, "[%s] Could not open \"%s\": %s\n",
							__func__, fname, strerror( errno ));
				}

			} else {
				frame->error_call( frame, "[%s] Expected string, but have %s\n",
						__func__, type_str( frame->expr->next->next->type ));
			}

		} else {
			frame->error_call( frame, "[%s] Expected string, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Need moar tokenz\n", __func__ );
	}

	return ret;
}

token_t *builtin_readall( stack_frame_t *frame ){
	token_t *ret = NULL;
	FILE *fp;
	shared_t *shr;
	char *dat;

	if ( frame->ntokens >= 2 ){
		if ( frame->expr->next->type == TYPE_FILE ){
			shr = frame->expr->next->data;
			fp = shared_get( shr );
			dat = read_input_file( fp );

			ret = alloc_token( );
			ret->type = TYPE_STRING;
			ret->data = shared_new( dat, free_string );

		} else {
			frame->error_call( frame, "[%s] Expected file, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Need moar tokenz\n", __func__ );
	}

	return ret;
}

token_t *builtin_read_char( stack_frame_t *frame ){
	token_t *ret = NULL;
	FILE *fp;
	shared_t *shr;
	int c;

	if ( frame->ntokens >= 2 ){
		if ( frame->expr->next->type == TYPE_FILE ){
			shr = frame->expr->next->data;
			fp = shared_get( shr );

			c = fgetc( fp );

			if ( !feof( fp )){
				ret = alloc_token( );
				ret->type = TYPE_CHAR;
				ret->smalldata = c;

			} else {
				ret = alloc_token( );
				ret->type = TYPE_BOOLEAN;
				ret->smalldata = false;
			}


		} else {
			frame->error_call( frame, "[%s] Expected file, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}

	} else {
		//frame->error_call( frame, "[%s] Need moar tokenz\n", __func__ );
		ret = alloc_token( );
		ret->type = TYPE_CHAR;
		ret->smalldata = getchar( );
	}

	return ret;
}

token_t *builtin_write_char( stack_frame_t *frame ){
	token_t *ret = NULL;
	FILE *fp;
	shared_t *shr;

	if ( frame->ntokens >= 3 ){
		if ( frame->expr->next->type == TYPE_FILE ){
			if ( frame->expr->next->next->type == TYPE_CHAR ){
				shr = frame->expr->next->data;
				fp = shared_get( shr );

				fputc( frame->expr->next->next->smalldata, fp );

				ret = alloc_token( );
				ret->type = TYPE_CHAR;
				ret->smalldata = frame->expr->next->next->smalldata;

			} else {
				frame->error_call( frame, "[%s] Expected char, but have %s\n",
					__func__, type_str( frame->expr->next->next->type ));
			}

		} else {
			frame->error_call( frame, "[%s] Expected file, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Need moar tokenz\n", __func__ );
	}

	return ret;
}

token_t *builtin_is_eof( stack_frame_t *frame ){
	token_t *ret = NULL;
	FILE *fp;
	int c;

	if ( frame->ntokens == 2 ){
		if ( frame->expr->next->type == TYPE_FILE ){
			fp = shared_get( frame->expr->next->data );
			ret = alloc_token( );
			ret->type = TYPE_BOOLEAN;
			ret->smalldata = 0;

			c = fgetc( fp );
			if ( c == EOF || !feof( fp )){
				ret->smalldata = 1;
			} else {
				ungetc( c, fp );
			}

			ret->smalldata = feof( fp ) != 0;

		} else {
			frame->error_call( frame, "[%s] Expected file, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}
	}

	return ret;
}

token_t *builtin_display( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	token_t *file_tok;
	FILE *fp;


	move = frame->expr->next;
	if ( move ){
		ret = alloc_token( );
		ret->type = TYPE_NULL;
		file_tok = move->next;

		if ( file_tok && file_tok->type == TYPE_FILE ){
			fp = shared_get( file_tok->data );
			file_print_token( fp, move );

		} else {
			file_print_token( stdout, move );
		}

	} else {
		frame->error_call(
			frame,
			"[%s] Error: expected 1 or 2 arguments, but have %u\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_newline( stack_frame_t *frame ){
	token_t *ret;

	ret = alloc_token( );
	ret->type = TYPE_NULL;

	if ( frame->expr->next ){
		FILE *fp = shared_get( frame->expr->next->data );
		fputc( '\n', fp );

	} else {
		putchar( '\n' );
	}

	return ret;
}
#endif
