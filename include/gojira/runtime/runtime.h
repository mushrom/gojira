#ifndef _GOJIRA_RUNTIME_RUNTIME_H
#define _GOJIRA_RUNTIME_RUNTIME_H 1
#include <gojira/tokens.h>
#include <gojira/libs/stack.h>
#include <gojira/libs/hashmap.h>
#include <gojira/tokens.h>

typedef struct variable {
	token_t *token;

	unsigned references;
} variable_t;

typedef struct continuation {
	struct continuation *last; // Pointer to previous continuation
	token_t *value;       // Where the end value of an expression is stored.
	token_t *ret;         // pointer to original place in code (return position)
	list_head_t *vars;    // Variable list, acts as the scope
} continuation_t;
typedef continuation_t cont_t;

token_t *eval_tokens( continuation_t *cont, token_t *tokens );
cont_t *cont_create( cont_t *cur_cont, token_t *ret_pos );

#endif
