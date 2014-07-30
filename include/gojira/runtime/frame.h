#ifndef _GOJIRA_RUNTIME_FRAME_H
#define _GOJIRA_RUNTIME_FRAME_H 1
#include <gojira/tokens.h>
#include <gojira/libs/list.h>

typedef struct variable {
	token_t *token;
	char *key;

	unsigned references;
} variable_t;

typedef struct stack_frame {
	struct stack_frame *last; // Pointer to previous frame
	token_t *ret;         // pointer to original place in code (return position)
	token_t *ptr;         // pointer to next token to evaluate

	list_head_t *vars;    // Variable list, acts as the scope
	token_t *expr;        // Token list built during evaluation
	token_t *end;         // Last token in the expression
	unsigned ntokens;     // Number of tokens in expr
	unsigned status;

	token_t *value;       // value to return to last continuation
} stack_frame_t;
typedef stack_frame_t st_frame_t;

st_frame_t *init_global_frame( st_frame_t *frame );
st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr );
st_frame_t *frame_free( st_frame_t *frame );
variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token );
token_t *frame_add_token( st_frame_t *frame, token_t *token );
token_t *frame_find_var( st_frame_t *frame, char *key );

void stack_trace( st_frame_t *frame );

#endif
