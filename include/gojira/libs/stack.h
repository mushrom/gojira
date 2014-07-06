#ifndef _GOJIRA_STACK_H
#define _GOJIRA_STACK_H
#include <gojira/libs/list.h>

typedef struct stack {
	list_head_t *stack;
	unsigned items;
} stack_t;

stack_t *stack_create( int flags );
void stack_free( stack_t *stack );

void *stack_push( stack_t *stack, void *item );
void *stack_pop( stack_t *stack );
void *stack_peek( stack_t *stack );

#endif
