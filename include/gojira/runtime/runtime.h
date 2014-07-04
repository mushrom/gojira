#ifndef _GOJIRA_RUNTIME_RUNTIME_H
#define _GOJIRA_RUNTIME_RUNTIME_H 1
#include <gojira/tokens.h>
#include <gojira/libs/list.h>
#include <gojira/libs/hashmap.h>

typedef struct variable {
	token_t *token;

	unsigned references;
} variable_t;

typedef struct runtime {
	unsigned flags;

	list_head_t *scope_stack;
	list_head_t *continuations;

	list_node_t *current_scope;
	list_node_t *current_cont;
} runtime_t;

token_t *eval_tokens( runtime_t *runtime, token_t *tokens );

#endif
