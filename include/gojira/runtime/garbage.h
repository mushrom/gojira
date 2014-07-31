#ifndef _GOJIRA_RUNTIME_GARBAGE_H
#define _GOJIRA_RUNTIME_GARBAGE_H
#include <gojira/tokens.h>

enum {
	GC_UNMARKED,
	GC_MARKED,
};

void gc_mark( token_t *tree );
void gc_unmark( token_t *tree );
token_t *gc_sweep( token_t *tree );

#endif
