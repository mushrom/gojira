#ifndef _GOJIRA_RUNTIME_RUNTIME_H
#define _GOJIRA_RUNTIME_RUNTIME_H 1
#include <gojira/tokens.h>
#include <gojira/libs/stack.h>
#include <gojira/libs/hashmap.h>
#include <gojira/tokens.h>

typedef struct variable {
	token_t *token;
	char *key;

	unsigned references;
} variable_t;

typedef struct stack_frame {
	struct stack_frame *last; // Pointer to previous frameinuation
	token_t *ret;         // pointer to original place in code (return position)
	list_head_t *vars;    // Variable list, acts as the scope

	token_t *list;        // token list built during evaluation
	token_t *end;         // last token in list
	unsigned ntokens;     // number of tokens in the list
	token_t *value;
} stack_frame_t;
typedef stack_frame_t st_frame_t;

token_t *eval_tokens( stack_frame_t *st_frame, token_t *tokens );
st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ret_pos );
variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token );

#endif
