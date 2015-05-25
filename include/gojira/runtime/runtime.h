#ifndef _GOJIRA_RUNTIME_RUNTIME_H
#define _GOJIRA_RUNTIME_RUNTIME_H 1
#ifdef __cplusplus
extern "C" {
#endif

#include <gojira/tokens.h>
#include <gojira/runtime/frame.h>
#include <gojira/libs/list.h>
#include <gojira/libs/hashmap.h>
#include <gojira/libs/shared.h>
#include <stdbool.h>

typedef struct binding {
	token_t *symbol;
	token_t *value;

	struct binding *next;
} binding_t;

typedef struct procedure {
	token_t *args;
	token_t *body;

	binding_t *bindings;
} procedure_t;

typedef struct iterator {
	shared_t *procedure;
	unsigned counter;
	unsigned limit;
} iterator_t;

token_t *eval_loop( stack_frame_t *base, token_t *tokens );
token_t *eval_loop_timed( stack_frame_t *base, token_t *tokens, unsigned limit );
bool eval_frame_subexpr( stack_frame_t **frame_ret, stack_frame_t *first );
bool eval_frame_expr( stack_frame_t **frame_ret, stack_frame_t *first );

#ifdef __cplusplus
}
#endif
#endif
