#include <stdio.h>

#include <gojira/tokens.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/shared.h>
#include <gojira/libs/dlist.h>
#include <gojira/runtime/files.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/printer.h>
#include <gojira/runtime/builtins/bytevector.h>

static void escaped_print_str( FILE *fp, const char *buf ){
	const char *s = buf;

	fputc( '"', fp );

	for ( ; *s; s++ ){
		switch (*s) {
			case '\\':
				fprintf( fp, "\\\\" );
				break;
			case '"':
				fprintf( fp, "\\\"" );
				break;
			default:
				fputc( *s, fp );
				break;
		}
	}

	fputc( '"', fp );
}

void file_print_token( FILE *fp, token_t *token, print_readable_t readable ){
	procedure_t *proc;
	shared_t *shr;

	if ( token && fp ){
		switch ( token->type ){
			case TYPE_NUMBER:
				fprintf( fp, "%ld", token->number.s_int );
				break;

			case TYPE_REAL:
				fprintf( fp, "%g", token->number.real );
				break;

			case TYPE_RATIONAL:
				fprintf( fp, "%ld/%ld", token->number.num, token->number.denom );
				break;

			case TYPE_BOOLEAN:
				fprintf( fp, "#%c", (token->boolean == true)? 't' : 'f' );
				break;

			case TYPE_STRING:
				if ( readable ){
					escaped_print_str( fp, (char *)shared_get( token->data ));
				} else {
					fprintf( fp, "%s", (char *)shared_get( token->data ));
				}

				break;

			case TYPE_SYMBOL:
				if ( readable ){
					fprintf( fp, "'%s", (char *)shared_get( token->data ));
				} else {
					fprintf( fp, "%s", (char *)shared_get( token->data ));
				}
				break;

			case TYPE_CHAR:
				if ( readable ){
					if ( token->character == '\n' ){
						fprintf( fp, "#\\newline" );
					} else if ( token->character == '\r' ){
						fprintf( fp, "#\\return" );
					} else if ( token->character == ' ' ){
						fprintf( fp, "#\\space" );
					} else if ( token->character == '\t' ){
						fprintf( fp, "#\\tab" );
					} else if ( token->character == '\x1b' ){
						fprintf( fp, "#\\escape" );
					} else if ( token->character == '\b' ){
						fprintf( fp, "#\\backspace" );
					} else {
						fprintf( fp, "#\\%c", token->character );
					}

				} else {
					fprintf( fp, "%c", token->character );
				}

				break;

			case TYPE_LIST:
				fputc( '(', fp );
				file_dump_tokens( fp, token->down, readable );
				fputc( ')', fp );
				break;

			case TYPE_PROCEDURE:
				shr = token->data;
				proc = shared_get( shr );

				fprintf( fp, "#<%s ", type_str( token->type ));
				file_print_token( fp, proc->args, readable );

#if GOJIRA_PUBLIC_MODE
				fprintf( fp, ">" );
#else
				fprintf( fp, " @ %p>", (void *)shr );
#endif
				break;

			case TYPE_VECTOR:
				shr = token->data;

				fprintf( fp, "#(" );

				if ( token->flags & T_FLAG_HAS_SHARED ){
					dlist_t *foo = shared_get( shr );
					unsigned i;

					foreach_in_dlist( i, foo ){
						if ( i > 0 )
							fputc( ' ', fp );

						file_dump_tokens( fp, dlist_get( foo, i ), readable );
					}
				}

				fprintf( fp, ")" );
				break;

			case TYPE_BYTEVECTOR:
				{
					bytevector_t *bytevec = shared_get( token->data );
					unsigned i;

					readable? fprintf( fp, "#vu8(" ) : 0;
					for ( i = 0; i < bytevec->length; i++ ){
						if ( readable && i > 0 )
							fputc( ' ', fp );

						fprintf( fp, readable? "#x%02x" : "%c", bytevec->bytes[i] );
					}
					readable? fprintf( fp, ")" ) : 0;
				}

				break;

			default:
				fprintf( fp, "#<%s>", type_str( token->type ));
				break;
		}
	}
}

// Prints all the tokens in a given tree
token_t *file_dump_tokens( FILE *fp, token_t *tokens, print_readable_t readable ){
	token_t *move;

	for ( move = tokens; move; move = move->next ){
		file_print_token( fp, move, readable );
		if ( move->next )
			fputc( ' ', fp );
	}

	return tokens;
}

token_t *dump_tokens( token_t *tokens ){
	return file_dump_tokens( stdout, tokens, true );
}

void print_token( token_t *token, print_readable_t readable ){
	file_print_token( stdout, token, readable );
}
