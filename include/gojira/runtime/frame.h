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
	RUNTIME_FLAG_NULL  = 0,
	RUNTIME_FLAG_TRACE = 1, // flag for function input/output tracing
	RUNTIME_FLAG_BREAK = 2, // signals that execution should stop
	                        // after this frame
};

enum variable_mutability {
	VAR_IMMUTABLE,
	VAR_MUTABLE,
};

enum toggle_environment {
	DONT_MAKE_ENV = false,
	MAKE_ENV      = true,
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

typedef struct environment {
	struct environment *last;
	hashmap_t *vars;
	unsigned refs;
} env_t;

typedef struct stack_frame {
	struct stack_frame *last; // Pointer to previous frame
	token_t *ret;         // pointer to original place in code (return position)
	token_t *ptr;         // pointer to next token to evaluate

	env_t *env;
	token_t *expr;        // Token list built during evaluation
	token_t *end;         // Last token in the expression
	unsigned ntokens;     // Number of tokens in expr
	unsigned status;
	unsigned flags;       // various runtime flags

	token_t *value;       // value to return to last continuation
	token_t *heap;        // List of all tokens allocated in the frame
	token_t *cur_func;

	gbg_collector_t gc;

	error_printer error_call;
} stack_frame_t;
typedef stack_frame_t st_frame_t;

//variable_t *global_add_func( st_frame_t *frame, char *name, scheme_func handle );
variable_t *global_add_func( env_t *env, char *name, scheme_func handle );
st_frame_t *init_global_frame( st_frame_t *frame );
st_frame_t *frame_create( st_frame_t *cur_frame, token_t *ptr, bool make_env );
st_frame_t *frame_free( st_frame_t *frame );
void env_release( env_t *env );
env_t *env_aquire( env_t *env );
env_t *env_create( env_t *last );

#define DEPRECATED __attribute__((deprecated))

token_t *frame_add_token( st_frame_t *frame, token_t *token );
token_t *frame_add_token_noclone( st_frame_t *frame, token_t *token );
token_t *frame_register_token_tree( st_frame_t *frame, token_t *token ) DEPRECATED;
token_t *frame_register_one_token( st_frame_t *frame, token_t *token ) DEPRECATED;
token_t *frame_register_tokens( st_frame_t *frame, token_t *token ) DEPRECATED;
token_t *frame_alloc_token( st_frame_t *frame ) DEPRECATED;

#undef DEPRECATED

//variable_t *frame_add_var( st_frame_t *frame, char *key, token_t *token, bool recurse );
     env_t *env_free_vars( env_t *env );
variable_t *env_add_var( env_t *env, const char *key, token_t *token, bool recurse, bool mutable );
   token_t *env_find_var( env_t *env, const char *key, bool recurse );
variable_t *env_find_var_struct( env_t *env, const char *key, bool recurse );
  shared_t *env_find_shared_struct( env_t *env, const char *key, bool recurse );

   token_t *env_find_var_hash( env_t *env, unsigned hash, bool recurse );
variable_t *env_find_var_struct_hash( env_t *env, unsigned hash, bool recurse );
  shared_t *env_find_shared_struct_hash( env_t *env, unsigned hash, bool recurse );

void default_error_printer( stack_frame_t *frame, char *fmt, ... );
void stack_trace( st_frame_t *frame );

#ifdef __cplusplus
}
#endif
#endif
