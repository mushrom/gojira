#include <gojira/libs/list.h>
#include <string.h>
#include <stdlib.h>

list_head_t *list_create( int flags ){
	list_head_t *ret = 0;
	ret = malloc( sizeof( list_head_t ));
	memset( ret, 0, sizeof( list_head_t ));
	ret->flags = flags;

	return ret;
}

void list_free( list_head_t *list ){
	if ( list ){
		list_free_nodes( list->base );
		free( list );
	}
}

list_node_t *list_add_int( list_head_t *list, int val ){
	list_node_t *ret;

	ret = list->last = list_add_int_node( list->last, val );
	if ( !list->base )
		list->base = list->last;
	
	return ret;
}

list_node_t *list_add_data( list_head_t *list, void *data ){
	list_node_t *ret;

	ret = list->last = list_add_data_node( list->last, data );
	if ( !list->base )
		list->base = list->last;
	
	return ret;
}

list_node_t *list_remove_index( list_head_t *list, int index ){
	list_node_t *ret = 0,
		    *move;

	move = list_get_index( list, index );
	if ( move ){
		if ( move == list->last )
			list->last = move->prev;

		if ( move == list->base )
			list->base = move->next;

		ret = list_remove_node( move );
	}

	return ret;
}

list_node_t *list_get_index( list_head_t *list, int i ){
	list_node_t *ret = list->base;
	int n = 0;

	if ( i >= 0 ){
		ret = list->base;
	} else if ( i < 0 ){
		ret = list->last;
		n = -1;
	}

	while ( ret && n != i ){
		if ( i > 0 ){
			n++;
			ret = ret->next;
		} else if ( i < 0 ){
			n--;
			ret = ret->prev;
		}
	}

	return ret;
}

list_node_t *list_get_val( list_head_t *list, int val ){
	list_node_t *ret = 0,
		    *move;

	move = list->base;
	foreach_in_list( move ){
		if ( move->val == val ){
			ret = move;
			break;
		}
	}

	return ret;
}

list_node_t *list_add_node( list_node_t *list, int val, void *data ){
	list_node_t *ret = 0,
		    *temp,
		    *move;

	temp = malloc( sizeof( list_node_t ));
	temp->val = val;
	temp->data = data;
	temp->prev = 0;
	temp->next = 0;
	ret = temp;

	if ( list ){
		for ( move = list; move->next; move = move->next );

		move->next = temp;
		move->next->prev = move;
		move->next->next = 0;

	} else {
		move = temp;
		move->next = 0;
		move->prev = 0;
	}

	return ret;
}

list_node_t *list_add_int_node( list_node_t *list, int val ){
	return list_add_node( list, val, 0 );
}

list_node_t *list_add_data_node( list_node_t *list, void *data ){
	return list_add_node( list, 0, data );
}

list_node_t *list_remove_node( list_node_t *node ){
	list_node_t *ret = 0;

	if ( node ){
		if ( node->prev ){
			node->prev->next = node->next;
			ret = node->prev;
		}

		if ( node->next ){
			node->next->prev = node->prev;
			ret = ret? ret : node->next;
		}

		free( node );
	}

	return ret;
}

unsigned listlen( list_node_t *node ){
	list_node_t *move = node;
	unsigned i;

	for ( i = 0; move; move = move->next )
		i++;

	return i;
}

void list_free_nodes( list_node_t *node ){
	list_node_t *move,
		    *temp;

	if ( node ){
		for ( move = node; move->prev; move = move->prev );
		for ( ; move; move = temp ){
			temp = move->next;
			free( move );
		}
	}
}

