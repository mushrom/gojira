#include <stdio.h>

#include <gojira/tokens.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/shared.h>
#include <gojira/libs/dlist.h>
#include <gojira/runtime/files.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/printer.h>

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

void file_print_token( FILE *fp, token_t *token ){
	procedure_t *proc;
	shared_t *shr;

	if ( token && fp ){
		switch ( token->type ){
			case TYPE_NUMBER:
				fprintf( fp, "%d", token->smalldata );
				break;

			case TYPE_BOOLEAN:
				fprintf( fp, "#%c", (token->smalldata == true)? 't' : 'f' );
				break;

			case TYPE_STRING:
				escaped_print_str( fp, (char *)shared_get( token->data ));
				break;

			case TYPE_SYMBOL:
				fprintf( fp, "'%s", (char *)shared_get( token->data ));
				break;

			case TYPE_CHAR:
				if ( token->smalldata == '\n' ){
					fprintf( fp, "#\\newline" );
				} else if ( token->smalldata == '\r' ){
					fprintf( fp, "#\\return" );
				} else if ( token->smalldata == ' ' ){
					fprintf( fp, "#\\space" );
				} else if ( token->smalldata == '\t' ){
					fprintf( fp, "#\\tab" );
				} else if ( token->smalldata == '\x1b' ){
					fprintf( fp, "#\\escape" );
				} else if ( token->smalldata == '\b' ){
					fprintf( fp, "#\\backspace" );
				} else {
					fprintf( fp, "#\\%c", token->smalldata );
				}
				break;

			case TYPE_LIST:
				fputc( '(', fp );
				file_dump_tokens( fp, token->down );
				fputc( ')', fp );
				break;

			case TYPE_PROCEDURE:
				shr = token->data;
				proc = shared_get( shr );

				fprintf( fp, "#<%s (", type_str( token->type ));
				file_dump_tokens( fp, proc->args );
				fprintf( fp, ") @ %p>", (void *)shr );
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

						file_dump_tokens( fp, dlist_get( foo, i ));
					}
				}

				fprintf( fp, ")" );
				break;

			default:
				fprintf( fp, "#<%s>", type_str( token->type ));
				break;
		}
	}
}

// Prints all the tokens in a given tree
token_t *file_dump_tokens( FILE *fp, token_t *tokens ){
	token_t *move;

	for ( move = tokens; move; move = move->next ){
		file_print_token( fp, move );
		if ( move->next )
			fputc( ' ', fp );
	}

	return tokens;
}

token_t *dump_tokens( token_t *tokens ){
	return file_dump_tokens( stdout, tokens );
}

void print_token( token_t *token ){
	file_print_token( stdout, token );
}
