#include <gojira/runtime/allocate.h>
#include <gojira/tokens.h>
#include <stdlib.h>

token_t *nodeheap = NULL;

token_t *alloc_token( void ){
	token_t *ret;

	if ( nodeheap ){
		if ( nodeheap->next ){
			token_t *temp = nodeheap->next;
			nodeheap->next = NULL;
			free_tokens( temp );
		}

		ret = nodeheap;
		nodeheap = nodeheap->down;

		ret->down = ret->gc_link = NULL;
		ret->status = 0;
		//memset( ret, 0, sizeof( token_t ));

	} else {
		ret = calloc( 1, sizeof( token_t ));
	}

	//printf( "Allocated node\n" );
	/*
	ret = calloc( 1, sizeof( token_t ));
	*/

	return ret;
}

void free_tokens( token_t *tree ){

	if ( tree ){
		//printf( "Freeing node\n" );

		if ( tree->down ){
			token_t *move;
			for ( move = tree; move->down; move = move->down );
			move->down = nodeheap;

		} else {
			tree->down = nodeheap;
		}

		nodeheap = tree;

		/*
		free_tokens( tree->down );
		free_tokens( tree->next );
		free( tree );
		*/

		// TODO: Free data field in tokens properly, will likely need reference tracking.
	}
}

void destroy_token_cache_nodes( token_t *tree ){
	token_t *move, *temp;

	move = tree;
	move->gc_link = NULL;

	while ( move ){
		if ( move->down ){
			temp = move->down->gc_link = move;
			move = move->down;

			temp->down = NULL;

		} else if ( move->next ){
			temp = move->next->gc_link = move;
			move = move->next;

			temp->next = NULL;

		} else {
			temp = move->gc_link;
			free( move );
			move = temp;
		}
	}
}

void destroy_token_cache( ){
	destroy_token_cache_nodes( nodeheap );
}

void free_token_tree( token_t *tree ){
	if ( tree ){
		tree->next = NULL;
		//free_tokens( tree->down );
		free_tokens( tree );
		//free( tree );
	}
}
