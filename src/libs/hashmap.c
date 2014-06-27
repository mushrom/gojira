#include <gojira/libs/hashmap.h>
#include <stdlib.h>

hashmap_t *hashmap_create( unsigned n ){
	hashmap_t *ret;

	ret = calloc( 1, sizeof( hashmap_t ));
	ret->buckets = calloc( 1, sizeof( list_head_t[n] ));
	ret->nbuckets = n;

	return ret;
}

void hashmap_free( hashmap_t *map ){
	if ( map ){
		free( map->buckets );
		free( map );
	}
}

void *hashmap_add( hashmap_t *map, unsigned hash, void *val ){
	list_head_t *list;
	list_node_t *node;
	void *ret = 0;
	
	list = map->buckets + ( hash % map->nbuckets );

	node = list_add_data( list, val );
	if ( node ){
		node->val = hash;
		ret = node->data;
	}

	return ret;
}

void *hashmap_get( hashmap_t *map, unsigned hash ){
	list_head_t *list;
	list_node_t *node;
	void *ret = 0;
	
	list = map->buckets + ( hash % map->nbuckets );
	node = list_get_val( list, hash );

	if ( node )
		ret = node->data;

	return ret;
}

void hashmap_remove( hashmap_t *map, unsigned hash ){
	list_head_t *list;
	list_node_t *node;
	
	list = map->buckets + ( hash % map->nbuckets );
	node = list_get_val( list, hash );

	if ( node )
		list_remove_node( node );
}
