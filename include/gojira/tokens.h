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
} type_t;

typedef struct token {
	type_t type;
	void *data;

	struct token *next;
	struct token *down;
} token_t;

#endif
