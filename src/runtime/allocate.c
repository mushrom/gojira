#include <gojira/runtime/allocate.h>
#include <gojira/tokens.h>
#include <stdlib.h>

enum alloc_config {
	MAX_TOKEN_CACHE = 0xffff,
};

token_t *nodeheap = NULL;
unsigned avail_tokens = 0;

token_t *alloc_token( void ){
	token_t *ret;

	if ( nodeheap ){
		ret = nodeheap;
		nodeheap = nodeheap->down;

		ret->down = ret->gc_link = NULL;
		ret->status = 0;
		avail_tokens--;

	} else {
		ret = calloc( 1, sizeof( token_t ));
	}

	return ret;
}

void cache_token( token_t *token ){
	token->next = NULL;
	token->down = nodeheap;
	avail_tokens++;

	nodeheap = token;
}

void print_avail( void ){
	printf( "Cached tokens: %u\n", avail_tokens );
}

void free_tokens( token_t *tree ){
	if ( tree ){
		free_tokens( tree->down );
		free_tokens( tree->next );

		if ( avail_tokens < MAX_TOKEN_CACHE ){
			cache_token( tree );

		} else {
			free( tree );
		}

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
