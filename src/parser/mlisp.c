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

static void expand_closing_parens( stack_t *stack, token_t **pos );
static void expand_mlisp_indent( stack_t *stack, token_t *pos );
static void expand_mlisp_newline( stack_t *stack, token_t *pos );

static inline token_t *make_token( type_t type, token_t *next ){
	//token_t *ret = calloc( 1, sizeof( token_t ));
	token_t *ret = alloc_block( );

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
	unsigned line_count = 0;

	stack_t *stack = stack_create( 0 );
	indent_pos_t *foo;

	for ( ; move ; move = move->next ){
		if ( move->type == TYPE_NEWLINE ){
			line_count++;

			if ( move->next ){
				token_t *next = move->next;

				if ( next->type == TYPE_NEWLINE || next->type == TYPE_NULL ){
					expand_mlisp_newline( stack, move );

				} else if ( next->type == TYPE_INDENT ){
					expand_mlisp_indent( stack, move );

				} else {
					expand_closing_parens( stack, &move );
					push_indent( stack, 0, 1 );
					insert_token( &move, TYPE_OPEN_PAREN );
				}
			}
		}

		if ( move->type == TYPE_COLON ){
			foo = stack_peek( stack );

			if ( foo ){
				foo->parens++;
			}

			insert_token( &move, TYPE_OPEN_PAREN );
		}
	}

	return ret;
}

static void expand_closing_parens( stack_t *stack, token_t **pos ){
	indent_pos_t *foo = NULL;

	if ( stack_peek( stack )){
		for ( foo = stack_pop( stack ); foo; foo = stack_pop( stack )){
			unsigned i = foo->parens;

			for ( ; i; i-- ){
				insert_token( pos, TYPE_CLOSE_PAREN );
			}
		}
	}
}

static void expand_mlisp_newline( stack_t *stack, token_t *pos ){
	token_t *next = pos->next;

	if ( !( next->next && next->next->type == TYPE_INDENT )){
		expand_closing_parens( stack, &pos );
	}
}

static void expand_mlisp_indent( stack_t *stack, token_t *pos ){
	indent_pos_t *foo = stack_peek( stack );
	bool period = (pos->next->next && pos->next->next->type == TYPE_PERIOD);

	if ( foo ) {
		// handle an indent larger than the last
		if ( foo->indent < pos->next->character ){
			foo = push_indent( stack, pos->next->character, period == false );

			if ( period == false ){ 
				insert_token( &pos, TYPE_OPEN_PAREN );
			}

		// handle a line with the same indentation as the last
		} else if ( foo->indent == pos->next->character ){
			unsigned i;

			for ( i = foo->parens; i; i-- ){
				insert_token( &pos, TYPE_CLOSE_PAREN );
			}

			if ( !period ) insert_token( &pos, TYPE_OPEN_PAREN );
			foo->parens = period == false;

		// handle a line with lower indentation than the last
		} else {
			foo = stack_pop( stack );

			while (foo && foo->indent != pos->next->character) {
				unsigned i;
				for ( i = foo->parens; i; i-- ){
					insert_token( &pos, TYPE_CLOSE_PAREN );
				}

				free( foo );
				foo = stack_pop( stack );
			}

			if ( foo ){
				unsigned i;
				for ( i = foo->parens; i; i-- ){
					insert_token( &pos, TYPE_CLOSE_PAREN );
				}

				insert_token( &pos, TYPE_OPEN_PAREN );
				foo->parens = period == false;
				stack_push( stack, foo );

			} else {
				printf( "[%s] unmatched indents", __func__ );
			}
		}
	}
}

token_t *parse_mlisp_tokens( char *buf ){
	return remove_punc_tokens( parse_tokens(
				//debug_print( remove_meta_tokens( preprocess_mlisp( lexerize( buf ))))));
				remove_meta_tokens( preprocess_mlisp( lexerize( buf )))));
}

