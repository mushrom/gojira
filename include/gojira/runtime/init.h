#ifndef _GOJIRA_RUNTIME_INIT_H
#define _GOJIRA_RUNTIME_INIT_H 1
#include <gojira/tokens.h>
#include <gojira/libs/list.h>
#include <gojira/libs/hashmap.h>

typedef struct variable {
	token_t *token;

	unsigned references;
} variable_t;

typedef struct runtime {
	token_t *token_tree;
	token_t *instr_ptr;
	
	list_head_t *scope_stack;
	list_node_t *current_scope;
} runtime_t;

runtime_t *initialize_runtime( unsigned flags, token_t *tree );
void deinit_runtime( unsigned flags, runtime_t *runtime );

#endif
