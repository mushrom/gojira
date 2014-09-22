#include <gojira/runtime/garbage.h>
#include <stdlib.h>
#include <stdbool.h>

void gc_mark( token_t *tree ){
	if ( tree && tree->status == GC_UNMARKED ){
		tree->status = GC_MARKED;

		gc_mark( tree->next );
		gc_mark( tree->down );
	}
}

void gc_unmark( token_t *tree ){
	if ( tree ){
		tree->status = GC_UNMARKED;

		gc_unmark( tree->next );
		gc_unmark( tree->down );
	}
}

token_t *gc_sweep( token_t *tree ){
	token_t *ret = NULL;

	if ( tree ){
		if ( tree->status == GC_MARKED ){
			tree->status = GC_UNMARKED;
			ret = tree->gc_link = gc_sweep( tree->gc_link );

		} else {
			ret = gc_sweep( tree->gc_link );
			free_token_tree( tree );
		}
	}

	return ret;
}

void gc_dump_tokens( token_t *token ){
	int i;
	token_t *move;

	for ( i = 0, move = token; move; move = move->gc_link, i++ ){
		printf( "[%s] Token %d\n", __func__, i );
		dump_tokens( move );
	}
}

void gc_dump( stack_frame_t *frame ){
	printf( "[%s] Garbage heap dump\n", __func__ );
	if ( frame->last == NULL )
		printf( "[%s] Is base frame\n", __func__ );

	gc_dump_tokens( frame->heap );
}
