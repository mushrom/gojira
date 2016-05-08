#include <gojira/config.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

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
		DEBUGP( "[%s] Closing file at %p\n", __func__, ptr );
		fclose( (FILE *)ptr );
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
					ret = gc_alloc_token( get_current_gc( frame ));
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

token_t *builtin_close( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *file = frame->expr->next;

		if ( file->type == TYPE_FILE ){
			shared_t *shr = file->data;
			FILE *fp = shared_get( shr );

			if ( fp ){
				fclose( fp );
				shr->data = NULL;

				ret = gc_alloc_token( get_current_gc( frame ));
				ret->type = TYPE_NULL;

			} else {
				FRAME_ERROR( frame, "trying to close an already closed file", 0 );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "file", file->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
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

			if ( fp ){
				dat = read_input_file( fp );

				ret = gc_alloc_token( get_current_gc( frame ));
				ret->type = TYPE_STRING;
				ret->data = shared_new( dat, free_string );

			} else {
				FRAME_ERROR( frame, "trying to read a closed file", 0 );
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

token_t *builtin_read_char( stack_frame_t *frame ){
	token_t *ret = NULL;
	FILE *fp;
	shared_t *shr;
	int c;

	if ( frame->ntokens >= 2 ){
		if ( frame->expr->next->type == TYPE_FILE ){
			shr = frame->expr->next->data;
			fp = shared_get( shr );

			if ( fp ){
				c = fgetc( fp );

				if ( !feof( fp )){
					ret = gc_alloc_token( get_current_gc( frame ));
					ret->type = TYPE_CHAR;
					ret->character = c;

				} else {
					ret = gc_alloc_token( get_current_gc( frame ));
					ret->type = TYPE_BOOLEAN;
					ret->boolean = false;
				}

			} else {
				FRAME_ERROR( frame, "trying to read a closed file", 0 );
			}

		} else {
			frame->error_call( frame, "[%s] Expected file, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}

	} else {
		//frame->error_call( frame, "[%s] Need moar tokenz\n", __func__ );
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_CHAR;
		ret->character = getchar( );
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

				if ( fp ){
					fputc( frame->expr->next->next->character, fp );
					ret = gc_alloc_token( get_current_gc( frame ));
					ret->type = TYPE_CHAR;
					ret->character = frame->expr->next->next->character;

				} else {
					FRAME_ERROR( frame, "trying to write to a closed file", 0 );
				}

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
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_BOOLEAN;
			ret->boolean = false;

			fp = shared_get( frame->expr->next->data );

			if ( fp ){
				c = fgetc( fp );
				if ( c == EOF || !feof( fp )){
					ret->boolean = true;
				} else {
					ungetc( c, fp );
				}

				ret->boolean = feof( fp ) != 0;

			} else {
				// if the file is closed, return true
				ret->boolean = true;
			}

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
	move = frame->expr->next;

	if ( move ){
		token_t *file_tok = move->next;

		if ( file_tok && file_tok->type == TYPE_FILE ){
			FILE *fp = shared_get( file_tok->data );

			if ( fp ){
				file_print_token( fp, move, OUTPUT_REGULAR );
				ret = gc_alloc_token( get_current_gc( frame ));
				ret->type = TYPE_NULL;

			} else {
				FRAME_ERROR( frame, "trying to write to a closed file", 0 );
			}

		} else {
			file_print_token( stdout, move, OUTPUT_REGULAR );
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NULL;
		}

	} else {
		frame->error_call(
			frame,
			"[%s] Error: expected 1 or 2 arguments, but have %u\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_write( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	move = frame->expr->next;

	if ( move ){
		token_t *file_tok = move->next;

		if ( file_tok && file_tok->type == TYPE_FILE ){
			FILE *fp = shared_get( file_tok->data );

			if ( fp ){
				file_print_token( fp, move, OUTPUT_READABLE );
				ret = gc_alloc_token( get_current_gc( frame ));
				ret->type = TYPE_NULL;

			} else {
				FRAME_ERROR( frame, "trying to write to a closed file", 0 );
			}

		} else {
			file_print_token( stdout, move, OUTPUT_READABLE );
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NULL;
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
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *file_tok = frame->expr->next;

		if ( file_tok->type == TYPE_FILE ){
			FILE *fp = shared_get( frame->expr->next->data );

			if ( fp ){
				fputc( '\n', fp );

				ret = gc_alloc_token( get_current_gc( frame ));
				ret->type = TYPE_NULL;

			} else {
				FRAME_ERROR( frame, "trying to write to a closed file", 0 );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "file", file_tok->type );
		}

	} else {
		putchar( '\n' );

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_NULL;
	}

	return ret;
}

static char *read_s_expr( FILE *fp ){
	unsigned pos, alloced, open, spaces;
	char *ret = malloc(1);
	char c = 0;

	open = pos = spaces = 0;
	alloced = 1;

	while ( !feof( fp )){
		c = fgetc( fp );

		if (!( c == ' ' || c == '\t' || c == '\n' || c == '\v' )) {
			ungetc( c, fp );
			break;
		}
	}

	for ( ; (spaces == 0 || open > 0) && !feof( fp ); pos++ ){
		if ( pos + 1 >= alloced ){
			alloced += 16;
			ret = realloc( ret, sizeof( char[ alloced + 1]));
		}

		c = fgetc( fp );

		if      ( c == '(' ) open++;
		else if ( c == ')' ) open--;
		else if ( c == ' ' ) spaces++;

		ret[pos] = c;
	}

	ret[pos] = 0;

	return ret;
}

token_t *builtin_read( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	char *buf;
	FILE *fp;

	move = frame->expr->next;
	if ( frame->ntokens == 2 ){
		if ( move->type == TYPE_FILE ){
			fp = shared_get( move->data );

			if ( fp ){
				buf = read_s_expr( fp );

				if ( strlen( buf ) != 0 ){
					ret = gc_register_tokens( get_current_gc( frame ), parse_scheme_tokens( buf ));

				} else {
					ret = gc_alloc_token( get_current_gc( frame ));
					ret->type = TYPE_BOOLEAN;
					ret->boolean = false;
				}

				free( buf );

			} else {
				FRAME_ERROR( frame, "trying to read an closed file", 0 );
			}

		} else {
			frame->error_call(
				frame,
				"[%s] Error: expected file, but have %s\n",
				__func__, type_str( move->type ));
		}

	} else {
		frame->error_call(
			frame,
			"[%s] Error: expected 1 argument, but have %u\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_mkdir( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *temp;
	int result;

	if ( frame->ntokens == 2 ){
		temp = frame->expr->next;
		if ( temp->type == TYPE_STRING ){
			result = mkdir( shared_get( temp->data ), 0755 );

			if ( result == 0 ){
				ret = gc_alloc_token( get_current_gc( frame ));
				ret->type = TYPE_BOOLEAN;
				ret->boolean = true;

			} else {
				frame->error_call( frame,
					"[%s] Error: could not create directory\n",
					__func__, frame->ntokens - 1 );
			}

		} else {
			frame->error_call( frame,
				"[%s] Error: expected string, but have %s\n",
				__func__, type_str( temp->type ));
		}

	} else {
		frame->error_call( frame,
			"[%s] Error: expected 1 argument, but have %u\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_file_exists( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *temp;
	int result;

	if ( frame->ntokens == 2 ){
		temp = frame->expr->next;
		if ( temp->type == TYPE_STRING ){
			result = access( shared_get( temp->data ), F_OK );

			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_BOOLEAN;
			ret->boolean = result == 0;

		} else {
			frame->error_call( frame,
				"[%s] Error: expected string, but have %s\n",
				__func__, type_str( temp->type ));
		}

	} else {
		frame->error_call( frame,
			"[%s] Error: expected 1 argument, but have %u\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

#endif
