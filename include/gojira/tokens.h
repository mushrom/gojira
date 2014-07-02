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

	// Types used by parser
	TYPE_BASE_TOKEN,
	TYPE_TOKEN_LIST,
	TYPE_QUOTED_TOKEN,
} type_t;

typedef struct token {
	type_t type;
	unsigned status;

	union {
		void *data;
		unsigned smalldata;
	};

	struct token *next;
	struct token *down;
} token_t;

token_t *dump_tokens( token_t *tokens, int level );

#endif
