#ifndef _GOJIRA_RUNTIME_GARBAGE_H
#define _GOJIRA_RUNTIME_GARBAGE_H
#include <stdbool.h>

// Debugging stuff, remove me eventually
//#include <gojira/runtime/frame.h>
#include <stdio.h>

enum {
	/*
	GC_COLOR_WHITE,
	GC_COLOR_BLACK,
	GC_COLOR_GREY,
	*/
	GC_UNMARKED,
	GC_MARKED,
	GC_FREED,
};

enum {
	GC_TYPE_TOKEN,
	GC_TYPE_ENVIRONMENT,
	GC_TYPE_CONTINUATION,
};

/*
typedef struct gbg_list {
	token_t *start;
	token_t *end;
	unsigned length;
} gbg_list_t;
*/

typedef struct gbg_node {
	struct gbg_node *next;
	struct gbg_node *prev;
	unsigned type;
	unsigned status;
	unsigned id;
} gbg_node_t;

typedef struct gbg_collector {
	//gbg_list_t colors[3];
	/*
	token_t *start;
	token_t *end;
	*/
	gbg_node_t *start;
	gbg_node_t *end;
	unsigned length;
	unsigned id;
	unsigned interval;
} gbg_collector_t;

#include <gojira/tokens.h>
typedef struct token token_t;

token_t *gc_alloc_token( gbg_collector_t *gc );
token_t *gc_clone_token( gbg_collector_t *gc, token_t *token );
void *gc_register( gbg_collector_t *gc, void *thing );
//token_t *gc_register_token( gbg_collector_t *gc, token_t *token );
token_t *gc_register_token_tree( gbg_collector_t *gc, token_t *tokens );
token_t *gc_register_tokens( gbg_collector_t *gc, token_t *token );
token_t *gc_move_token( gbg_collector_t *to, gbg_collector_t *from, token_t *token );
void gc_free_token( gbg_collector_t *gc );
void gc_collect( gbg_collector_t *gc );
bool gc_should_collect( gbg_collector_t *gc );
gbg_collector_t *gc_init( gbg_collector_t *old_gc, gbg_collector_t *new_gc );
gbg_collector_t *gc_merge( gbg_collector_t *first, gbg_collector_t *second );

typedef struct stack_frame stack_frame_t;
void gc_try_to_collect_frame( stack_frame_t *frame );
gbg_collector_t *get_current_gc( stack_frame_t *frame );

#define DEPRECATED __attribute__((deprecated))

void gc_mark( token_t *tree ) DEPRECATED;
void gc_mark_tree( token_t *tree ) DEPRECATED;
void gc_unmark( token_t *tree ) DEPRECATED;
void gc_unmark_tree( token_t *tree ) DEPRECATED;
token_t *gc_sweep( token_t *tree ) DEPRECATED;

//void gc_dump( stack_frame_t *frame );

#undef DEPRECATED

#endif
