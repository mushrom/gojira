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
	env_t   *env;

	binding_t *bindings;
} procedure_t;

typedef struct iterator {
	shared_t *procedure;
	unsigned counter;
	// possible methods for generating the next iterator
	type_t next_type;
	union {
		unsigned limit;
		shared_t *nproc;
		shared_t *iter;
	};
} iterator_t;

typedef enum {
	EVAL_STATUS_NONE,
	EVAL_STATUS_ERROR,
	EVAL_STATUS_RUNNING,
} eval_ret_t;

void free_string( void *ptr );

eval_ret_t eval_step( stack_frame_t **frame );
token_t *eval_loop( stack_frame_t *base );
token_t *eval_loop_timed( stack_frame_t *base, unsigned limit );
bool eval_frame_subexpr( stack_frame_t **frame_ret );
bool eval_frame_expr( stack_frame_t **frame_ret );

#ifdef __cplusplus
}
#endif
#endif
