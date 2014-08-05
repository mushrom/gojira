#include <gojira/runtime/allocate.h>
#include <gojira/tokens.h>
#include <stdlib.h>

token_t *nodeheap = NULL;

token_t *alloc_token( void ){
	token_t *ret;

	if ( nodeheap ){
		ret = nodeheap;
		nodeheap = nodeheap->down;

		ret->down = ret->gc_link = NULL;
		ret->status = 0;

	} else {
		ret = calloc( 1, sizeof( token_t ));
	}

	return ret;
}

void cache_token( token_t *token ){
	token->next = NULL;
	token->down = nodeheap;

	nodeheap = token;
}

void free_tokens( token_t *tree ){
	if ( tree ){
		free_tokens( tree->down );
		free_tokens( tree->next );
		cache_token( tree );

		// TODO: Free data field in tokens properly,
		// will likely need reference tracking.
	}
}

void destroy_token_cache_nodes( token_t *tree ){
	token_t *move, *temp;

	for ( move = tree; move; move = temp ){
		temp = move->down;
		free( move );
	}
}

void destroy_token_cache( ){
	destroy_token_cache_nodes( nodeheap );
}

void free_token_tree( token_t *tree ){
	if ( tree ){
		tree->next = NULL;
		free_tokens( tree );
	}
}
