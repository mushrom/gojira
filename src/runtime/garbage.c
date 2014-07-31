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
