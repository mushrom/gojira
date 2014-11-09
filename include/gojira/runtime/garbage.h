#ifndef _GOJIRA_RUNTIME_GARBAGE_H
#define _GOJIRA_RUNTIME_GARBAGE_H
#include <gojira/tokens.h>

// Debugging stuff, remove me eventually
#include <gojira/runtime/frame.h>
#include <stdio.h>

enum {
	GC_UNMARKED,
	GC_MARKED,
	GC_FREED,
};

void gc_mark( token_t *tree );
void gc_unmark( token_t *tree );
token_t *gc_sweep( token_t *tree );

void gc_dump( stack_frame_t *frame );

#endif
