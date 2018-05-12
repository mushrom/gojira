#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/hashmap.h>
#include <gojira/libs/shared.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static void free_hashmap_token( void *ptr ){
	hashmap_t *map = ptr;
	list_node_t *node;
	list_node_t *temp;
	unsigned i;

	for ( i = 0; i < map->nbuckets; i++ ){
		node = map->buckets[i].base;

		for ( ; node; node = temp ){
			temp = node->next;
			free( node );
		}
	}

	hashmap_free( map );
}

token_t *builtin_hashmap_make( stack_frame_t *frame ){
	token_t *ret = NULL;
	hashmap_t *map = NULL;

	token_t *move = frame->expr->next;
	char *str;

	map = hashmap_create(32);

	for ( ; move; (move = move->next) && (move = move->next) ){
		if ( move->type == TYPE_SYMBOL || move->type == TYPE_STRING ){
			str = shared_get( move->data );
			hashmap_add( map, hash_string( str ), move->next );

		} else {
			hashmap_free( map );
			map = NULL;

			FRAME_ERROR_ARGTYPE(frame, "symbol or string",
								frame->expr->next->type);
		}
	}

	if ( map ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_HASHMAP;
		ret->flags = T_FLAG_HAS_SHARED;
		ret->data = shared_new( map, free_hashmap_token );
	}

	return ret;
}

token_t *builtin_hashmap_get( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		if ( frame->expr->next->type == TYPE_HASHMAP ){
			if ( frame->expr->next->next->type == TYPE_SYMBOL ||
					frame->expr->next->next->type == TYPE_STRING )
			{
				token_t *sym = frame->expr->next->next;
				char *buf = shared_get( sym->data );
				hashmap_t *map = shared_get( frame->expr->next->data );
				token_t *temp = hashmap_get( map, hash_string( buf ));

				if ( temp ){
					ret = gc_clone_token( get_current_gc( frame ), temp );

				} else {
					ret = gc_alloc_token( get_current_gc( frame ));
					ret->type = TYPE_BOOLEAN;
					ret->boolean = false;
				}

			} else {
				FRAME_ERROR_ARGTYPE( frame, "symbol",
				                     frame->expr->next->type );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "symbol",
			                     frame->expr->next->type );
		}
	} else {
		FRAME_ERROR_ARGNUM(frame, 2);
	}

	return ret;
}

token_t *builtin_hashmap_set( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 4 ){
		if ( frame->expr->next->type == TYPE_HASHMAP ){
			if ( frame->expr->next->next->type == TYPE_SYMBOL ||
					frame->expr->next->next->type == TYPE_STRING )
			{
				token_t *sym = frame->expr->next->next;
				char *buf = shared_get( sym->data );
				hashmap_t *map = shared_get( frame->expr->next->data );

				hashmap_set( map, hash_string(buf),
				             frame->expr->next->next->next );
			} else {
				FRAME_ERROR_ARGTYPE( frame, "symbol",
				                     frame->expr->next->type );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "hashmap",
			                     frame->expr->next->type );
		}

	} else {
		FRAME_ERROR_ARGNUM(frame, 3);
	}

	ret = gc_alloc_token( get_current_gc( frame ));
	ret->type = TYPE_BOOLEAN;
	ret->boolean = true;

	return ret;
}
