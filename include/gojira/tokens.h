#ifndef _GOJIRA_TOKENS_H
#define _GOJIRA_TOKENS_H 1

typedef enum {
	TYPE_NULL,
	TYPE_BOOLEAN,
	TYPE_NUMBER,
	TYPE_CHAR,
	TYPE_STRING,
	TYPE_SYMBOL,
	TYPE_PAIR,
	TYPE_LIST,
	TYPE_VECTOR,

	TYPE_OPEN_PAREN,
	TYPE_CLOSE_PAREN,
	TYPE_PERIOD,
	TYPE_APOSTR,
	TYPE_OCTOTHORPE,
	TYPE_ASTERISK,

	// Generated from symbol names in lexer
	TYPE_LAMBDA,
	TYPE_IF,
	TYPE_DEF_SYNTAX,
	TYPE_SYNTAX_RULES,

	// Types used by parser
	TYPE_BASE_TOKEN,
	TYPE_TOKEN_LIST,
	TYPE_QUOTED_TOKEN,
	TYPE_PROCEDURE,

	// Types used by interpreter
	TYPE_EXTERN_PROC,
	TYPE_SYNTAX,
} type_t;

typedef struct token {
	type_t type;

	// Used for rule reduction while parsing, and for garbage status
	// during runtime.
	unsigned status;

	// token data
	union {
		void *data;
		unsigned smalldata;
	};

	struct token *next;
	struct token *down;

	// Used in frames to keep (seperate) token list of all allocated tokens
	struct token *gc_link;
} token_t;

void print_token( token_t *token );
token_t *dump_tokens( token_t *tokens );
token_t *strip_token( token_t *tokens, type_t type );
token_t *remove_punc_tokens( token_t *tokens );

token_t *clone_token( token_t *token );
token_t *clone_tokens( token_t *tree );
token_t *clone_token_tree( token_t *tree );
token_t *clone_token_spine( token_t *tree );

unsigned tokens_length( token_t *tree );
token_t *replace_symbol( token_t *tokens, token_t *replace, char *name );
token_t *replace_symbol_safe( token_t *tokens, token_t *replace, char *name );

token_t *debug_print( token_t *tokens );

#include <gojira/runtime/allocate.h>

#endif
