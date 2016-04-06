#ifndef _GOJIRA_TOKENS_H
#define _GOJIRA_TOKENS_H 1
#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>

typedef enum {
	TYPE_NULL,

	// Basic types
	TYPE_BOOLEAN,
	TYPE_NUMBER,
	TYPE_RATIONAL,
	TYPE_REAL,
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
	TYPE_COLON,

	// Generated from symbol names in lexer
	TYPE_LAMBDA,
	TYPE_IF,
	TYPE_DEF_SYNTAX,
	TYPE_SYNTAX_RULES,

	// Generated as 'metadata' from the lexer
	TYPE_NEWLINE,
	TYPE_INDENT,

	// Types used by parser
	TYPE_BASE_TOKEN,
	TYPE_TOKEN_LIST,
	TYPE_QUOTED_TOKEN,
	TYPE_PROCEDURE,

	// Types used by interpreter for internal housekeeping
	TYPE_EXTERN_PROC,
	TYPE_SYNTAX,
	TYPE_VARIABLE_REF,
	TYPE_CONTINUATION,

	// Types for custom extensions
	TYPE_FILE,
	TYPE_ITERATOR,
	TYPE_SOCKET,
	TYPE_HASHMAP,

	// "less basic" types
	TYPE_BYTEVECTOR,

	TYPE_END,
} type_t;

typedef enum {
	T_FLAG_NULL       = 0,
	T_FLAG_HAS_SHARED = 1,
} token_flag_t;

// stack_frame_t defined in include/gojira/runtime/frame.h
typedef struct stack_frame stack_frame_t;
typedef struct token token_t;
typedef struct gbg_node gbg_node_t;
typedef token_t *(*scheme_func)( stack_frame_t * );

#include <gojira/libs/numbers.h>
#include <gojira/runtime/garbage.h>

typedef struct token {
	gbg_node_t gc_link;

	struct token *next;
	struct token *down;

	// Used in frames to keep (seperate) token list of all allocated tokens
	//struct token *gc_link;
	/*
	struct token *gc_prev;
	struct token *gc_next;
	*/
	//void *gc_data;

	// token data
	union {
		void *data;
		scheme_func func;
		struct number_buf number;
		uint8_t byte;
		uint32_t character;
		uint32_t misc;
		bool boolean;
		stack_frame_t *cont;
	};

	type_t type;
	unsigned gc_id;
	unsigned status;    // Used for rule reduction while parsing, and for
	                    // garbage status during runtime.
	token_flag_t flags; // special flags about the token
} token_t;

token_t *strip_token( token_t *tokens, type_t type );
token_t *remove_token_list( token_t *tokens, type_t remove[], unsigned n );
token_t *remove_punc_tokens( token_t *tokens );
token_t *remove_meta_tokens( token_t *tokens );

bool has_shared_data( type_t type );
token_t *clone_token( const token_t *token );
token_t *clone_tokens( const token_t *tree );
token_t *clone_token_tree( const token_t *tree );
token_t *clone_token_spine( const token_t *tree );

unsigned tokens_length( const token_t *tree );
token_t *replace_symbol( token_t *tokens, const token_t *replace, const char *name );
token_t *replace_symbol_safe( token_t *tokens, const token_t *replace, const char *name );
token_t *replace_type( token_t *tokens, const token_t *replace, type_t type );

token_t *parse_scheme_tokens( char *buf );

const token_t *debug_print( const token_t *tokens );

static inline bool has_number_type( token_t *token ){
	return token->type == TYPE_NUMBER
		|| token->type == TYPE_REAL
		|| token->type == TYPE_RATIONAL;
}

#include <gojira/runtime/allocate.h>

#ifdef __cplusplus
}
#endif
#endif
