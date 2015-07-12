#ifndef _GOJIRA_RUNTIME_FRAME_H
#define _GOJIRA_RUNTIME_FRAME_H 1
#ifdef __cplusplus
extern "C" {
#endif

#include <gojira/tokens.h>
#include <gojira/libs/list.h>
#include <gojira/libs/hashmap.h>
#include <gojira/libs/shared.h>
#include <stdbool.h>

enum recurse_vals {
	NO_RECURSE = false,
	RECURSE    = true
};

enum variable_mutability {
	VAR_IMMUTABLE,
	VAR_MUTABLE,
};

typedef struct variable {
	token_t *token;
	char *key;

	unsigned references;
	unsigned hash;
	bool is_mutable;
} variable_t;

struct stack_frame;
typedef void (*error_printer)( struct stack_frame *, char *fmt, ... );

typedef struct stack_frame {
	struct stack_frame *last; // Pointer to previous frame
	token_t *ret;         // pointer to original place in code (return position)
	token_t *ptr;         // pointer to next token to evaluate

	//list_head_t *vars;    // Variable list, acts as the scope
	hashmap_t *vars;    // Variable list, acts as the scope
	token_t *expr;        // Token list built during evaluation
	token_t *end;         // Last token in the expression
	unsigned ntokens;     // Number of tokens in expr
	unsigned status;

	token_t *value;       // value to return to last continuation
	token_t *heap;        // List of all tokens allocated in the frame

	error_printer error_call;
} stack_frame_t;
typedef stack_frame_t st_frame_t;

variable_t *global_add_func( st_frame_t *frame, char *name, scheme_func handle );
st_frame_t *init_global_frame( st_frame_t *frame );
st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr );
st_frame_t *frame_free( st_frame_t *frame );

token_t *frame_add_token( st_frame_t *frame, token_t *token );
token_t *frame_add_token_noclone( st_frame_t *frame, token_t *token );
token_t *frame_register_token_tree( st_frame_t *frame, token_t *token );
token_t *frame_register_one_token( st_frame_t *frame, token_t *token );
token_t *frame_register_tokens( st_frame_t *frame, token_t *token );
token_t *frame_alloc_token( st_frame_t *frame );

//variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token, bool recurse );
variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token, bool recurse, bool mutable );
variable_t *frame_find_var_struct( st_frame_t *frame, char *key, bool recurse );
token_t *frame_find_var( st_frame_t *frame, char *key, bool recurse );
shared_t *frame_find_shared_struct( st_frame_t *frame, char *key, bool recurse );

void default_error_printer( stack_frame_t *frame, char *fmt, ... );
void stack_trace( st_frame_t *frame );

#ifdef __cplusplus
}
#endif
#endif
