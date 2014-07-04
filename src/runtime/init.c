#include <gojira/runtime/init.h>
#include <stdlib.h>

runtime_t *initialize_runtime( unsigned flags ){
	runtime_t *ret;

	ret = calloc( 1, sizeof( runtime_t ));

	ret->flags = flags;
	ret->scope_stack = list_create( 0 );
	list_add_data( ret->scope_stack, hashmap_create( 16 ));
	ret->current_scope = list_get_index( ret->scope_stack, 0 );

	return ret;
}

void deinit_runtime( runtime_t *runtime ){
	list_node_t *temp;

	temp = list_get_index( runtime->scope_stack, 0 );
	foreach_in_list( temp )
		free( temp->data );

	free( runtime->scope_stack );
	free( runtime );
}
