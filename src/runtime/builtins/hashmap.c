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
			free_tokens( node->data );
			free( node );
		}
	}

	hashmap_free( map );
}

token_t *builtin_hashmap_make( stack_frame_t *frame ){
	token_t *ret = NULL;
	hashmap_t *map = NULL;

	if ( frame->ntokens >= 3 ){
		token_t *move = frame->expr->next;
		char *str;

		map = hashmap_create( 4 );

		for ( ; move; (move = move->next) && (move = move->next) ){
			if ( move->type == TYPE_SYMBOL || move->type == TYPE_STRING ){
				str = shared_get( move->data );
				hashmap_add( map, hash_string( str ), clone_token_tree( move->next ));

			} else {
				frame->error_call(
					frame,
					"[%s] Error: Expected symbol or string as key, but have %s\n",
					__func__, type_str( move->type ));

				hashmap_free( map );
				map = NULL;
			}
		}

		if ( map ){
			ret = alloc_token( );
			ret->type = TYPE_HASHMAP;
			ret->flags = T_FLAG_HAS_SHARED;
			ret->data = shared_new( map, free_hashmap_token );
		}

	} else {
		frame->error_call(
			frame,
			"[%s] Error: Expected at least 2 arguments, but have %d\n",
			__func__, frame->ntokens - 1 );
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
				//token_t *map_tok = frame->expr->next;
				token_t *sym = frame->expr->next->next;
				char *buf = shared_get( sym->data );
				hashmap_t *map = shared_get( frame->expr->next->data );
				token_t *temp = hashmap_get( map, hash_string( buf ));

				if ( temp ){
					ret = clone_token_tree( temp );

				} else {
					ret = alloc_token( );
					ret->type = TYPE_BOOLEAN;
					ret->smalldata = false;
				}

			} else {
				frame->error_call(
					frame,
					"[%s] Error: Expected symbol, but have %s\n",
					__func__, type_str( frame->expr->next->next->type ));
			}

		} else {
			frame->error_call(
				frame,
				"[%s] Error: Expected hashmap, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}
	} else {
		frame->error_call(
			frame,
			"[%s] Error: Expected at least 2 arguments, but have %d\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}
