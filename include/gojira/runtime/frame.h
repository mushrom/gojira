#ifndef _GOJIRA_RUNTIME_FRAME_H
#define _GOJIRA_RUNTIME_FRAME_H 1
#ifdef __cplusplus
extern "C" {
#endif

#include <gojira/tokens.h>
#include <gojira/runtime/garbage.h>
#include <gojira/libs/list.h>
#include <gojira/libs/hashmap.h>
#include <gojira/libs/shared.h>
#include <stdbool.h>

enum recurse_vals {
	NO_RECURSE = false,
	RECURSE    = true
};

enum runtime_flags {
	RUNTIME_FLAG_NULL     = 0,
	RUNTIME_FLAG_TRACE    = 1, // flag for function input/output tracing
	RUNTIME_FLAG_BREAK    = 2, // signals that execution should stop
	                           // after this frame
	RUNTIME_FLAG_CAPTURED = 4, // indicates that the current frame has been captured
	                           // as part of a continuation and shouldn't be changed
	RUNTIME_FLAG_NO_EVAL  = 8, // don't evaluate the expression in this frame
	                           // used when loading files
};

enum variable_mutability {
	VAR_IMMUTABLE,       // variable can't be changed
	VAR_MUTABLE,         // variable can be changed
	VAR_MUTABLE_BUILTIN, // variable can be changed, but with a warning if changed
};

enum toggle_environment {
	DONT_MAKE_ENV = false,
	MAKE_ENV      = true,
};

typedef struct variable {
	gbg_node_t gc_link;
	struct variable *next;

	token_t *token;
	char *key;

	//unsigned references;
	unsigned hash;
	unsigned is_mutable;
} variable_t;

struct stack_frame;
typedef void (*error_printer)( struct stack_frame *, char *fmt, ... );

typedef struct environment {
	gbg_node_t gc_link;
	gbg_collector_t *garbage;
	struct environment *last;

	variable_t *vars;
	//hashmap_t *vars;
	//unsigned refs;
} env_t;

#include <stdint.h>

typedef struct stack_frame {
	gbg_node_t gc_link;
	gbg_collector_t *garbage;

	token_t *expr;            // reverse token list built during evaluation
	token_t *end;             // Last token in the expression
	token_t *ptr;             // pointer to next token to evaluate
	token_t *value;           // value to return to last continuation

	env_t *env;
	struct stack_frame *last; // Pointer to previous frame
	error_printer error_call;

	unsigned ntokens;         // Number of tokens in expr
	uint16_t status;
	uint16_t flags;           // various runtime flags
} stack_frame_t;
typedef stack_frame_t st_frame_t;

//variable_t *global_add_func( st_frame_t *frame, char *name, scheme_func handle );
variable_t *global_add_func( stack_frame_t *frame, char *name, scheme_func handle );
st_frame_t *init_global_frame( st_frame_t *frame );
st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr, bool make_env );
st_frame_t *frame_free( st_frame_t *frame );

st_frame_t *frame_capture( stack_frame_t *frame );
st_frame_t *frame_restore( stack_frame_t *frame );
//void env_release( env_t *env );
//env_t *env_aquire( env_t *env );
//env_t *env_create( env_t *last );
env_t *env_create( gbg_collector_t *gc, env_t *last );
void env_free( env_t *env );

#define DEPRECATED __attribute__((deprecated))

token_t *frame_add_token( st_frame_t *frame, token_t *token );
token_t *frame_add_token_noclone( st_frame_t *frame, token_t *token );
token_t *frame_register_token_tree( st_frame_t *frame, token_t *token ) DEPRECATED;
token_t *frame_register_one_token( st_frame_t *frame, token_t *token ) DEPRECATED;
token_t *frame_register_tokens( st_frame_t *frame, token_t *token ) DEPRECATED;
token_t *frame_alloc_token( st_frame_t *frame ) DEPRECATED;

#undef DEPRECATED

//variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token, bool recurse );
//     env_t *env_free_vars( env_t *env );
variable_t *env_add_var( env_t *env, const char *key, token_t *token, bool recurse, unsigned mutable );
   token_t *env_find_var( env_t *env, const char *key, bool recurse );
variable_t *env_find_var_struct( env_t *env, const char *key, bool recurse );
//  shared_t *env_find_shared_struct( env_t *env, const char *key, bool recurse );

   token_t *env_find_var_hash( env_t *env, unsigned hash, bool recurse );
variable_t *env_find_var_struct_hash( env_t *env, unsigned hash, bool recurse );
//  shared_t *env_find_shared_struct_hash( env_t *env, unsigned hash, bool recurse );

void default_error_printer( stack_frame_t *frame, char *fmt, ... );
void stack_trace( st_frame_t *frame );

#ifdef __cplusplus
}
#endif
#endif
