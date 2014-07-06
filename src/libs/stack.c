#include <gojira/libs/stack.h>
#include <stdlib.h>

stack_t *stack_create( int flags ){
	stack_t *ret = NULL;

	ret = malloc( sizeof( stack_t ));
	ret->stack = list_create( 0 );
	ret->items = 0;

	return ret;
}

void stack_free( stack_t *stack ){
	if ( stack ){
		list_free( stack->stack );
		free( stack );
	}
}

void *stack_push( stack_t *stack, void *item ){
	list_add_data( stack->stack, item );

	return item;
}

void *stack_pop( stack_t *stack ){
	void *ret = NULL;
	list_node_t *node;

	node = list_get_index( stack->stack, -1 );
	if ( node ){
		ret = node->data;
		list_remove_index( stack->stack, -1 );
	}

	return ret;
}

void *stack_peek( stack_t *stack ){
	void *ret = NULL;
	list_node_t *node;

	node = list_get_index( stack->stack, -1 );
	if ( node ){
		ret = node->data;
	}

	return ret;
}
