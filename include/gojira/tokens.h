#ifndef _GOJIRA_TOKENS_H
#define _GOJIRA_TOKENS_H 1
#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

typedef enum {
	TYPE_NULL,

	// Basic types
	TYPE_BOOLEAN,
	TYPE_NUMBER,
	TYPE_CHAR,
	TYPE_STRING,
	TYPE_SYMBOL,
	TYPE_PAIR,
	TYPE_LIST,
	TYPE_VECTOR,

	// Punctuation
	TYPE_OPEN_PAREN,
	TYPE_CLOSE_PAREN,
	TYPE_PERIOD,
	TYPE_APOSTR,
	TYPE_OCTOTHORPE,
	TYPE_ASTERISK,
	TYPE_COMMA,
	TYPE_SEMICOLON,

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

	// Types used by interpreter for internal housekeeping
	TYPE_EXTERN_PROC,
	TYPE_SYNTAX,
	TYPE_VARIABLE_REF,
	TYPE_FILE,

	TYPE_END,
} type_t;

typedef enum {
	T_FLAG_NULL       = 0,
	T_FLAG_HAS_SHARED = 1,
} token_flag_t;

// stack_frame_t defined in include/gojira/runtime/frame.h
typedef struct stack_frame stack_frame_t;
typedef struct token token_t;
typedef token_t *(*scheme_func)( stack_frame_t * );

typedef struct token {
	type_t type;
	// Used for rule reduction while parsing, and for garbage status
	// during runtime.
	unsigned status;
	// special flags about the token
	token_flag_t flags;

	// token data
	union {
		void *data;
		unsigned smalldata;
		scheme_func func;
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

bool has_shared_data( type_t type );
token_t *clone_token( token_t *token );
token_t *clone_tokens( token_t *tree );
token_t *clone_token_tree( token_t *tree );
token_t *clone_token_spine( token_t *tree );

unsigned tokens_length( token_t *tree );
token_t *replace_symbol( token_t *tokens, token_t *replace, char *name );
token_t *replace_symbol_safe( token_t *tokens, token_t *replace, char *name );
token_t *replace_type( token_t *tokens, token_t *replace, type_t type );

token_t *debug_print( token_t *tokens );

#include <gojira/runtime/allocate.h>

#ifdef __cplusplus
}
#endif
#endif
