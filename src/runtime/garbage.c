#include <gojira/runtime/garbage.h>
#include <stdlib.h>
#include <stdbool.h>

void gc_mark( token_t *tree ){
	if ( tree ){
		tree->status = GC_MARKED;
		gc_mark( tree->next );
		gc_mark( tree->down );
	}
}

void gc_mark_tree( token_t *tree ){
	if ( tree ){
		tree->status = GC_MARKED;
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

void gc_unmark_tree( token_t *tree ){
	if ( tree ){
		tree->status = GC_UNMARKED;
		gc_unmark( tree->down );
	}
}

token_t *gc_link( token_t *heap, token_t *tree ){
	token_t *ret = heap;

	if ( heap ){
		tree->gc_link = heap;
		ret = tree;
	}

	return ret;
}

token_t *free_all_unmarked( token_t *heap, token_t *tree ){
	token_t *ret = heap;

	if ( tree ){
		ret = free_all_unmarked( ret, tree->down );
		ret = free_all_unmarked( ret, tree->next );

		if ( tree->status == GC_UNMARKED ){
			free_token( tree );

		} else {
			ret = gc_link( ret, tree );
		}
	}

	return ret;
}

token_t *gc_sweep( token_t *tree ){
	token_t *move = tree;
	token_t *last = NULL;
	token_t *temp;
	token_t *ret = NULL;

	while ( move ){
		switch( move->status ){
			// don't free and unmark if marked as in-use
			case GC_MARKED:
				last = move;
				if ( !ret ){
					ret = move;
				}

				move->status = GC_UNMARKED;
				move = move->gc_link;
				break;

			// free the token if not in use, and clean up heap
			case GC_UNMARKED:
				temp = move->gc_link;
				if ( last ){
					last->gc_link = temp;
				}
				free_token( move );
				move = temp;
				break;

			// otherwise just continue, for experimental GC statuses
			default:
				last = move;
				if ( !ret ){
					ret = move;
				}

				move = move->gc_link;
				break;
		}
	}

	return ret;
}

token_t *old_gc_sweep( token_t *tree ){
	token_t *ret = NULL;

	if ( tree ){
		if ( tree->status == GC_MARKED ){
			tree->status = GC_UNMARKED;
			ret = tree->gc_link = gc_sweep( tree->gc_link );

		} else {
			ret = gc_sweep( tree->gc_link );
			free_token( tree );
		}
	}

	return ret;
}

#if GOJIRA_DEBUG
#include <gojira/runtime/printer.h>
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
#endif
