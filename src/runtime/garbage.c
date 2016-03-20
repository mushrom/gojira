#include <gojira/runtime/garbage.h>
#include <gojira/tokens.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

static token_t *gc_list_add( gbg_collector_t *gc, unsigned color, token_t *token ){
	gbg_list_t *list = &gc->colors[color];

	if ( list->start && !list->end ){
		printf( "[%s] Have a list with a start but no end\n", __func__ );
	}

	token->gc_id = gc->id;
	token->gc_next = list->start;
	token->gc_prev = NULL;
	//token->gc_data = gc;
	token->status = color;

	list->start = token;
	list->length++;

	if ( token->gc_next ){
		token->gc_next->gc_prev = token;
	}
	/*
	if ( list->start ){
		list->start->gc_prev = token;
		//gc->white->gc_prev = token;
	}
	*/

	if ( !list->end ){
		list->end = token;
	}

	if ( list->start->gc_prev != NULL ){
		printf( "[%s] Have a start with tokens after it...\n", __func__ );
	}

	if ( list->end->gc_next != NULL ){
		printf( "[%s] Have an end with tokens after it...\n", __func__ );
		//for ( ; list->end->gc_next; list->end = list->end->gc_next );
	}

	if ( token->gc_next == token ){
		printf( "[%s] token %p->gc_next is itself???\n", __func__, token );
	}

	if ( token->gc_prev == token ){
		printf( "[%s] token %p->gc_prev is itself???\n", __func__, token );
	}

	return token;
}

static token_t *gc_list_remove( gbg_collector_t *gc, token_t *token ){
	gbg_list_t *list = &gc->colors[token->status];

	//printf( "[%s] removing %p, %u, start = %p, end = %p\n", __func__, token, gc->id, list->start, list->end );

	if ((!list->start && !list->end) || token->status >= 3 || list->length == 0 ){
		printf( "[%s] Got bad list remove request, token: %p, color: %u\n",
			__func__, token, token->status );

		printf( "\tToken color: %u, token: %p, prev: %p, next: %p, token gc: %u, gc id: %u\n",
			__func__, token->status, token, token->gc_prev, token->gc_prev,
			token->gc_id, gc->id );
		return token;
	}

	if ( token->gc_next == token ){
		printf( "[%s] token %p->gc_next is itself???\n", __func__, token );
	}

	if ( token->gc_prev == token ){
		printf( "[%s] token %p->gc_prev is itself???\n", __func__, token );
	}

	if ( token == list->start ){
		list->start = token->gc_next;
		//printf( "[%s] Set new list start, %p\n", __func__, list->start );
	}

	if ( token == list->end ){
		if ( token->gc_prev ){
			list->end = token->gc_prev;
		} else {
			list->end = list->start;
		}

		//printf( "[%s] Set new list end, %p\n", __func__, list->start );
	}

	/*
	if ( list->end && list->end->gc_next != NULL ){
		printf( "[%s] end is not the real end...?, %p\n", __func__, list->end->gc_next );
	}
	*/

	if ( token->gc_prev ){
		if ( token->status != token->gc_prev->status ){
			printf( "[%s] Warning! token is lying about which list it's in, according to previous!\n", __func__ );
			printf( "\tToken color: %u, real: %u, token: %p, prev: %p, token gc: %u, gc id: %u\n",
				__func__, token->status, token->gc_next->status, token, token->gc_prev,
				token->gc_id, gc->id );

			if ( token->gc_prev->gc_next != token ){
				printf( "\tPrevious token %p in color list does not point to %p, actually points to %p...\n",
						token->gc_prev, token, token->gc_prev->gc_next );
			}
		}

		//printf( "[%s] Set new %p->next to %p\n", __func__, token->gc_prev, token->gc_next );

		token->gc_prev->gc_next = token->gc_next;
	}

	if ( token->gc_next ){
		if ( token->status != token->gc_next->status ){
			printf( "[%s] Warning! token is lying about which list it's in, according to next!\n", __func__ );
			printf( "\tToken color: %u, real: %u, token: %p, next: %p, token gc: %u, gc id: %u\n",
				__func__, token->status, token->gc_next->status, token, token->gc_next,
				token->gc_id, gc->id );
		}

		if ( token->gc_next->gc_prev != token ){
			printf( "\tNext token %p in color list does not point to %p, actually points to %p... %d\n",
				token->gc_next, token, token->gc_next->gc_prev, token != token->gc_next->gc_prev );
		}

		//printf( "[%s] Set new %p->prev to %p\n", __func__, token->gc_next, token->gc_prev );

		token->gc_next->gc_prev = token->gc_prev;
	}

	token->gc_prev = token->gc_next = NULL;

	list->length--;

	return token;
}

static token_t *gc_list_move( gbg_collector_t *gc, unsigned color, token_t *token ){
	token_t *ret = token;

	if ( token->status < 3 && gc->id >= gc->id ){
		gc_list_remove( gc, token );
		gc_list_add( gc, color, token );

	} else {
		printf( "[%s] Invalid move request\n", __func__ );
		printf( "\tToken color: %u, token: %p, next: %p, token gc: %u, gc id: %u\n",
			__func__, token->status, token, token->gc_next, token->gc_id, gc->id );
	}

	return ret;
}

token_t *gc_alloc_token( gbg_collector_t *gc ){
	//token_t *ret = gc_list_add( gc, GC_COLOR_WHITE, alloc_token( ));
	token_t *ret = gc_register_token( gc, alloc_token( ));

	//printf( "[%s] Allocated token at %p\n", __func__, ret );

	return ret;
}

token_t *gc_clone_token( gbg_collector_t *gc, token_t *token ){
	//token_t *ret = gc_list_add( gc, GC_COLOR_WHITE, clone_token( token ));
	token_t *temp = clone_token( token );
	//temp->gc_id = gc->id;
	//token_t *ret = gc_register_token( gc, clone_token( token ));
	//printf( "[%s] Cloned %s token at %p\n", __func__, type_str( temp->type ), temp );
	token_t *ret = gc_register_token( gc, temp );


	return ret;
}
#include <signal.h>

token_t *gc_register_token( gbg_collector_t *gc, token_t *token ){
	token_t *ret = NULL;

	if ( token->gc_id && token->gc_id < gc->id ){
		printf( "[%s] Warning: asking for token registered at %u to be registered into %u\n", __func__, token->gc_id, gc->id );
		raise( SIGINT );
		ret = token;

	} else {
		ret = gc_list_add( gc, GC_COLOR_WHITE, token );
		//token_t *ret = gc_list_add( gc, GC_COLOR_WHITE, token );
	}


	//printf( "[%s] Registered a %s token at %p\n", __func__, type_str( token->type ), token );

	return ret;
}

token_t *gc_register_tokens( gbg_collector_t *gc, token_t *tokens ){
	token_t *ret = tokens;

	if ( tokens ){
		gc_register_tokens( gc, tokens->down );
		gc_register_tokens( gc, tokens->next );
		gc_register_token( gc, tokens );
		ret = tokens;
	}

	return ret;
}

token_t *gc_register_token_tree( gbg_collector_t *gc, token_t *tokens ){
	token_t *ret = tokens;

	if ( tokens ){
		gc_register_tokens( gc, tokens->down );
		gc_register_token( gc, tokens );
	}

	return ret;
}

void gc_free_token( gbg_collector_t *gc ){
	// TODO
}

token_t *gc_move_token( gbg_collector_t *to, gbg_collector_t *from, token_t *token ){
	token_t *ret = NULL;

	gc_list_remove( from, token );
	gc_register_token( to, token );

	return ret;
}

static void gc_color_tokens( gbg_collector_t *gc, unsigned color, token_t *tokens ){
	token_t *move = tokens;

	for ( ; move; move = move->next ){
		if ( move->gc_id >= gc->id ){
			gc_list_move( gc, color, move );
		} /*else {
			printf( "[%s] Somehow got a token from a lower stack frame, %d at gc %d\n",
				__func__, move->gc_id, gc->id );
		}
		*/
	}
}

void gc_collect( gbg_collector_t *gc, token_t *root_nodes, unsigned iters ){
	//gc_grey_tokens( gc, root_nodes );
	gc_color_tokens( gc, GC_COLOR_GREY, root_nodes );

	unsigned max_iters = (iters > 0)? iters : UINT_MAX;
	unsigned i;

	//printf( "[%s] doing garbage collection...\n", __func__ );

	for ( i = 0; i < max_iters; i++ ){
		token_t *foo = gc->colors[GC_COLOR_GREY].start;

		if ( foo ){
			//printf( "[%s] %u: blackened a %s token at %p\n", __func__, gc->id, type_str( foo->type ), foo );
			gc_list_move( gc, GC_COLOR_BLACK, foo );

				//gc_grey_tokens( gc, foo->down );
			gc_color_tokens( gc, GC_COLOR_GREY, foo->down );

		} else {
			break;
		}
	}

	if ( gc->colors[GC_COLOR_GREY].start == NULL ){
		token_t *move = gc->colors[GC_COLOR_WHITE].start;

		for ( ; move; move = gc->colors[GC_COLOR_WHITE].start ){
			if ( move->gc_id < gc->id ){
				printf( "[%s] Somehow got a token from a lower stack frame, %d at gc %d\n",
						__func__, move->gc_id, gc->id );
			}

			gc_list_remove( gc, move );
			//printf( "[%s] %u: Freed a %s token at %p\n", __func__, gc->id, type_str( move->type ), move );
			free_token( move );
		}

		gc_color_tokens( gc, GC_COLOR_WHITE, gc->colors[GC_COLOR_BLACK].start );

		/*
		move = gc->colors[GC_COLOR_BLACK].start;
		for ( ; move; move = gc->colors[GC_COLOR_BLACK].start ){
			gc_list_move( gc, GC_COLOR_WHITE, move );
			printf( "[%s] %u: Moving used %s token at %p back to white list\n", __func__, gc->id, type_str( move->type ), move );
		}
		*/
	}

	//printf( "[%s] done\n", __func__ );
}

bool gc_should_collect( gbg_collector_t *gc ){
	gc->interval++;

	if ( gc->colors[GC_COLOR_WHITE].length > 200 && gc->interval > 1000 ){
	//if ( gc->id == 1 ){
		gc->interval = 0;
		return true;

	} else {
		return false;
	}

	//return gc->colors[GC_COLOR_WHITE].length > 50;
}

gbg_collector_t *gc_init( gbg_collector_t *old_gc, gbg_collector_t *new_gc ){
	if ( old_gc ){
		new_gc->id = old_gc->id + 1;

	} else {
		new_gc->id = 1;
	}

	unsigned i;
	for ( i = 0; i < 3; i++ ){
		new_gc->colors[i].start  = NULL;
		new_gc->colors[i].end    = NULL;
		new_gc->colors[i].length = 0;
	}

	new_gc->interval = 0;

	/*
	new_gc->white = new_gc->black = new_gc->grey = NULL;
	new_gc->white_end = new_gc->black_end = new_gc->grey_end = NULL;
	new_gc->white_length = new_gc->black_length = new_gc->grey_length = 0;
	*/

	return new_gc;
}

gbg_collector_t *gc_merge( gbg_collector_t *first, gbg_collector_t *second ){
	unsigned i;

	for ( i = 0; i < 3; i++ ){
		gbg_list_t *a = &first->colors[i];
		gbg_list_t *b = &second->colors[i];

		if ( a->start ){
			if ( b->start ){
				b->start->gc_prev = a->end;
				a->end->gc_next = b->start;
				a->end = b->end;
			}

			if ( a->end ){
				if ( a->end->gc_next == a->end ){
					printf( "[%s] token %p->end->gc_next is itself???\n", __func__, a->end );
				}

				if ( a->end->gc_next != NULL ){
					printf( "[%s] end is not the real end? %p\n", __func__, a->end );
				}

			} else {
				printf( "[%s] Have start, but no end? first id: %u, second id: %u\n", __func__, first->id, second->id );
			}

			a->length += b->length;

		} else {
			a->start  = b->start;
			a->end    = b->end;
			a->length = b->length;
		}

		b->start = b->end = NULL;
		b->length = 0;
	}

	first->interval += second->interval;

	return first;
}

/*
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
*/

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
