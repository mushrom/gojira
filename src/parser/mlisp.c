#include <gojira/lexer.h>
#include <gojira/parser.h>
#include <gojira/config.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/files.h>
#include <gojira/libs/stack.h>
#include <stdlib.h>

typedef struct indent_pos {
	unsigned indent;
	unsigned parens;
} indent_pos_t;

token_t *make_token( type_t type, token_t *next ){
	token_t *ret = calloc( 1, sizeof( token_t ));

	ret->type = type;
	ret->next = next;

	return ret;
}

static inline void insert_token( token_t **foo, type_t type ){
	if ( foo && *foo ){
		(*foo)->next = make_token( type, (*foo)->next );
		*foo = (*foo)->next;
	}
}

static inline indent_pos_t *push_indent( stack_t *stack, unsigned indent, unsigned parens ){
	indent_pos_t *pos = calloc( 1, sizeof( indent_pos_t ));

	*pos = (indent_pos_t){
		.indent = indent,
		.parens = parens
	};

	stack_push( stack, pos );

	return pos;
}

token_t *preprocess_mlisp( token_t *tokens ){
	token_t *move = tokens;
	token_t *ret = tokens;

	stack_t *stack = stack_create( 0 );
	indent_pos_t *foo;

	for ( ; move ; move = move->next ){
		if ( move->type == TYPE_NEWLINE ){

			if ( move->next ){
				if ( move->next->type == TYPE_NEWLINE || move->next->type == TYPE_NULL ){

					if ( stack_peek( stack )){
						for ( foo = stack_pop( stack ); foo; foo = stack_pop( stack )){
							unsigned i = foo->parens;

							for ( ; i; i-- ){
								insert_token( &move, TYPE_CLOSE_PAREN );
							}
						}
					}

				} else if ( move->next->type == TYPE_INDENT ){
					foo = stack_peek( stack );
					bool period = (move->next->next && move->next->next->type == TYPE_PERIOD);

					if ( foo->indent < move->next->smalldata ){
						foo = push_indent( stack, move->next->smalldata, period == false );

						if ( foo->parens ){
							insert_token( &move, TYPE_OPEN_PAREN );
						}

					} else if ( foo->indent == move->next->smalldata ){
						unsigned i;

						for ( i = foo->parens; i; i-- ){
							insert_token( &move, TYPE_CLOSE_PAREN );
						}

						if ( !period ){
							insert_token( &move, TYPE_OPEN_PAREN );
						}

						foo->parens = period == false;

					} else {
						foo = stack_pop( stack );

						while (foo) {
							if ( foo->indent == move->next->smalldata ){
								unsigned i;
								for ( i = foo->parens; i; i-- ){
									insert_token( &move, TYPE_CLOSE_PAREN );
								}
							} 

							if ( foo->indent != move->next->smalldata ){
								free( foo );
								foo = stack_pop( stack );

							} else {
								break;
							}
						}

						if ( !foo ){
							printf( "[%s] Error: unmatched indents or something", __func__ );
						}
					}

				} else if ( move->next->type != TYPE_NEWLINE && move->next->type != TYPE_NULL ){
					push_indent( stack, 0, 1 );
					insert_token( &move, TYPE_OPEN_PAREN );
				}
			}
		}

		if ( move->type == TYPE_COLON ){
			foo = stack_peek( stack );
			foo->parens++;

			insert_token( &move, TYPE_OPEN_PAREN );
		}
	}

	return ret;
}


token_t *parse_mlisp_tokens( char *buf ){
	return remove_punc_tokens( parse_tokens(
				remove_meta_tokens( preprocess_mlisp( lexerize( buf )))));
}

