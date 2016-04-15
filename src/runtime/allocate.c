#include <gojira/runtime/allocate.h>
#include <gojira/runtime/frame.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct memblock {
	union {
		stack_frame_t frame;
		token_t       token;
		env_t         env;
	};
} memblock_t;

enum alloc_config {
	// about 16MB with 128 byte blocks
	// TODO: add an option to tweak this from the command line
	MAX_TOKEN_CACHE = 0x1ffff,
	//MAX_TOKEN_CACHE = 0,
};

//token_t *nodeheap = NULL;
unsigned avail_tokens = 0;
gbg_list_t nodeheap = {
	.start  = NULL,
	.end    = NULL,
	.length = 0
};

static inline void free_block_data( gbg_node_t *node ){
	if ( node->type == GC_TYPE_TOKEN ){
		token_t *token = (token_t *)node;

		if ( token->flags & T_FLAG_HAS_SHARED ){
			shared_release( token->data );
		}

	} else if ( node->type == GC_TYPE_VARIABLE ){
		variable_t *var = (variable_t *)node;

		free( var->key );
	}

	/*
	else if ( node->type == GC_TYPE_ENVIRONMENT ){
		env_t *env = (env_t *)node;

		env_free_vars( env );
	}
	*/
}

void *alloc_block( void ){
	//token_t *ret;
	void *ret;

	if ( nodeheap.length ){
		ret = nodeheap.start;
		free_block_data( ret );

		nodeheap.start = nodeheap.start->next;
		nodeheap.length--;

		memset( ret, 0, sizeof( memblock_t ));

		if ( nodeheap.length == 0 ){
			nodeheap.start = nodeheap.end = NULL;
		}

	} else {
		//printf( "%u\n", sizeof( memblock_t ));
		ret = calloc( 1, sizeof( memblock_t ));
	}

	return ret;
}

token_t *alloc_token( void ){
	token_t *ret = alloc_block( );

	ret->gc_link.type = GC_TYPE_TOKEN;

	return ret;
}

void cache_blocks( gbg_list_t *list ){
	if ( nodeheap.start && list->start ){
		nodeheap.end->next = list->start;
		list->start->prev = nodeheap.end;
		nodeheap.end = list->end;
		nodeheap.length += list->length;

	} else if ( list->start ){
		nodeheap.start  = list->start;
		nodeheap.end    = list->end;
		nodeheap.length = list->length;
	}

	while ( nodeheap.length > MAX_TOKEN_CACHE ){
		gbg_node_t *temp = nodeheap.start;
		nodeheap.start = nodeheap.start->next;
		nodeheap.length--;

		free_node( temp );
	}
}

void print_avail( void ){
	printf( "Cached tokens: %u\n", avail_tokens );
}

void free_node( gbg_node_t *node ){
	free_block_data( node );

	free( node );
}

void free_token( token_t *token ){
	free_node( &token->gc_link );
}

void free_tokens( token_t *tree ){
	if ( tree ){
		free_tokens( tree->down );
		free_tokens( tree->next );

		free_token( tree );
	}
}

void free_token_tree( token_t *tree ){
	if ( tree ){
		tree->next = NULL;
		free_tokens( tree );
	}
}

void destroy_token_cache( ){
	gbg_node_t *temp;
	gbg_node_t *foo;

	for ( temp = nodeheap.start; temp; temp = foo ){
		foo = temp->next;
		free_node( temp );
	}

	nodeheap = (gbg_list_t){
		.start  = NULL,
		.end    = NULL,
		.length = 0,
	};
}
