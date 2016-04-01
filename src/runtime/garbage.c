#include <gojira/runtime/garbage.h>
#include <gojira/tokens.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

static gbg_node_t *gc_list_add( gbg_collector_t *gc, gbg_node_t *node ){
	//gbg_list_t *list = &gc->colors[color];

	if ( !gc ){
		printf( "[%s] Given a null GC, things will break\n", __func__ );
	}

	if ( gc->start && !gc->end ){
		printf( "[%s] Have a list with a start but no end\n", __func__ );
	}

	//node->id = gc->id;
	node->next = gc->start;
	node->prev = NULL;
	//node->gc_data = gc;
	//node->status = color;

	//list->start = node;
	gc->start = node;
	gc->length++;

	if ( node->next ){
		node->next->prev = node;
	}

	//if ( list->start ){
	//	list->start->gc_prev = node;
	//	gc->white->gc_prev = node;
	//}

	if ( !gc->end ){
		gc->end = node;
	}

	if ( gc->start->prev != NULL ){
		printf( "[%s] Have a start with nodes after it...\n", __func__ );
	}

	if ( gc->end->next != NULL ){
		printf( "[%s] Have an end with nodes after it...\n", __func__ );
		//for ( ; gc->end->gc_next; gc->end = gc->end->gc_next );
	}

	if ( node->next == node ){
		printf( "[%s] node %p->gc_next is itself???\n", __func__, node );
	}

	if ( node->prev == node ){
		printf( "[%s] node %p->gc_prev is itself???\n", __func__, node );
	}

	return node;
}

static gbg_node_t *gc_list_remove( gbg_collector_t *gc, gbg_node_t *node ){
	//gbg_list_t *list = &gc->colors[node->status];

	//printf( "[%s] removing %p, %u, start = %p, end = %p\n", __func__, node, gc->id, list->start, list->end );

	if ((!gc->start && !gc->end) || node->status >= GC_MARKED || gc->length == 0 ){
		printf( "[%s] Got bad list remove request, node: %p, color: %u\n",
			__func__, node, node->status );

		printf( "\tnode color: %u, node: %p, prev: %p, next: %p, node gc: %u, gc id: %u\n",
			__func__, node->status, node, node->prev, node->prev,
			node->id, gc->id );
		return node;
	}

	if ( node->next == node ){
		printf( "[%s] node %p->next is itself???\n", __func__, node );
	}

	if ( node->prev == node ){
		printf( "[%s] node %p->prev is itself???\n", __func__, node );
	}

	if ( node == gc->start ){
		gc->start = node->next;
		//printf( "[%s] Set new list start, %p\n", __func__, list->start );
	}

	if ( node == gc->end ){
		if ( node->prev ){
			gc->end = node->prev;
		} else {
			gc->end = gc->start;
		}

		//printf( "[%s] Set new list end, %p\n", __func__, list->start );
	}

	if ( node->prev ){
		if ( node->status != node->prev->status ){
			/*
			printf( "[%s] Warning! node is lying about which list it's in, according to previous!\n", __func__ );
			printf( "\tnode color: %u, real: %u, node: %p, prev: %p, node gc: %u, gc id: %u\n",
				__func__, node->status, node->next->status, node, node->prev,
				node->gc_id, gc->id );
				*/

		}

		if ( node->prev->next != node ){
			printf( "\tPrevious node %p in color list does not point to %p, actually points to %p...\n",
					node->prev, node, node->prev->next );
		}

		//printf( "[%s] Set new %p->next to %p\n", __func__, node->prev, node->next );

		node->prev->next = node->next;
	}

	if ( node->next ){
		/*
		if ( node->status != node->next->status ){
			printf( "[%s] Warning! node is lying about which list it's in, according to next!\n", __func__ );
			printf( "\tnode color: %u, real: %u, node: %p, next: %p, node gc: %u, gc id: %u\n",
				__func__, node->status, node->next->status, node, node->next,
				node->gc_id, gc->id );
		}
		*/

		if ( node->next->prev != node ){
			printf( "\tNext node %p in color list does not point to %p, actually points to %p... %d\n",
				node->next, node, node->next->prev, node != node->next->prev );
		}

		//printf( "[%s] Set new %p->prev to %p\n", __func__, node->next, node->prev );

		node->next->prev = node->prev;
	}

	node->prev = node->next = NULL;

	gc->length--;

	return node;
}

/*
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
*/

token_t *gc_alloc_token( gbg_collector_t *gc ){
	//token_t *ret = gc_list_add( gc, GC_COLOR_WHITE, alloc_token( ));
	token_t *ret = gc_register( gc, alloc_token( ));

	//printf( "[%s] Allocated token at %p\n", __func__, ret );

	return ret;
}

token_t *gc_clone_token( gbg_collector_t *gc, token_t *token ){
	token_t *ret = NULL;

	if ( token ){
		token_t *temp = clone_token( token );
		//temp->gc_id = gc->id;
		//token_t *ret = gc_register_token( gc, clone_token( token ));
		//printf( "[%s] Cloned %s token at %p\n", __func__, type_str( temp->type ), temp );
		ret = gc_register( gc, temp );
	}

	return ret;
}
#include <signal.h>

void *gc_register( gbg_collector_t *gc, void *thing ){
	gbg_node_t *node = thing;

	node->status = GC_UNMARKED;

	gc_list_add( gc, node );

	//return node;
	return thing;
}

/*
token_t *gc_register_token( gbg_collector_t *gc, token_t *token ){
	token_t *ret = NULL;

	//if ( token->gc_id && token->gc_id < gc->id ){
	if ( !token->gc_id ){
		//raise( SIGINT );
		//ret = token;
		ret = gc_list_add( gc, &token->gc_link );

	} else {
		printf( "[%s] Warning: asking for token already registered %u to be registered into %u\n",
			__func__, token->gc_id, gc->id );

		ret = token;
		//token_t *ret = gc_list_add( gc, GC_COLOR_WHITE, token );
	}


	//printf( "[%s] Registered a %s token at %p\n", __func__, type_str( token->type ), token );

	return ret;
}
*/

token_t *gc_register_tokens( gbg_collector_t *gc, token_t *tokens ){
	token_t *ret = tokens;

	if ( tokens ){
		gc_register_tokens( gc, tokens->down );
		gc_register_tokens( gc, tokens->next );
		//gc_register_token( gc, tokens );
		gc_register( gc, tokens );
		ret = tokens;
	}

	return ret;
}

token_t *gc_register_token_tree( gbg_collector_t *gc, token_t *tokens ){
	token_t *ret = tokens;

	if ( tokens ){
		gc_register_tokens( gc, tokens->down );
		gc_register( gc, tokens );
	}

	return ret;
}

void gc_free_token( gbg_collector_t *gc ){
	// TODO
}

/*
token_t *gc_move_token( gbg_collector_t *to, gbg_collector_t *from, token_t *token ){
	token_t *ret = NULL;

	gc_list_remove( from, token );
	gc_register_token( to, token );

	return ret;
}
*/

#include <gojira/runtime/frame.h>
#include <gojira/runtime/runtime.h>

void gc_mark_env( gbg_collector_t *gc, env_t *env );
void gc_mark_envs( gbg_collector_t *gc, env_t *env );

void gc_mark_tokens( gbg_collector_t *gc, token_t *tokens ){
	token_t *move = tokens;

	for ( ; move && move->gc_link.status == GC_UNMARKED; move = move->next ){
	//for ( ; move; move = move->next ){
		//if ( move->gc_id >= gc->id ){
			//gc_list_move( gc, move );
			bool already_marked = move->gc_link.status == GC_MARKED;
			move->gc_link.status = GC_MARKED;

			if ( move->type == TYPE_PROCEDURE ){
				//printf( "[%s] Got here\n", __func__ );
				procedure_t *proc = shared_get( move->data );

				//if ( !already_marked ){
					gc_mark_envs( gc, proc->env );
					gc_mark_tokens( gc, proc->body );
					gc_mark_tokens( gc, proc->args );
				//}
			}

			gc_mark_tokens( gc, move->down );

		//}
	/*else {
			printf( "[%s] Somehow got a token from a lower stack frame, %d at gc %d\n",
				__func__, move->gc_id, gc->id );
		}
		*/
	}
}

void gc_mark_env( gbg_collector_t *gc, env_t *env ){
	if ( env && env->gc_link.status != GC_MARKED ){
		env->gc_link.status = GC_MARKED;

		if ( env->vars ){
			unsigned i;
			hashmap_t *map = env->vars;

			for ( i = 0; i < map->nbuckets; i++ ){
				list_node_t *node = map->buckets[i].base;

				foreach_in_list( node ){
					variable_t *var = shared_get( node->data );
					//gc_color_tokens( gc, GC_COLOR_GREY, var->token );
					gc_mark_tokens( gc, var->token );
				}
			}
		}
	}
}

static void free_gbg_node( gbg_node_t *node ){
	switch( node->type ){
		case GC_TYPE_TOKEN:
			free_token( (token_t *)node );
			break;

		case GC_TYPE_ENVIRONMENT:
			env_free( (env_t *)node );
			break;

		default:
			break;
	}
}

void gc_collect( gbg_collector_t *gc ){
	gbg_node_t *temp;
	gbg_node_t *foo;

	//gc_grey_gbg_nodes( gc, root_nodes );
	//gc_color_gbg_nodes( gc, GC_COLOR_GREY, root_nodes );
	//gc_mark_gbg_nodes( gc, root_nodes );

	temp = gc->start;

	while ( temp ){
		switch ( temp->status ){
			case GC_UNMARKED:
				foo = temp->next;
				gc_list_remove( gc, temp );
				free_gbg_node( temp );
				temp = foo;

				break;

			case GC_MARKED:
				temp->status = GC_UNMARKED;
				temp = temp->next;
				break;

			default:
				temp = temp->next;
				break;
		}
	}


	//unsigned max_iters = (iters > 0)? iters : UINT_MAX;
	//unsigned i;

	//printf( "[%s] doing garbage collection...\n", __func__ );
	//printf( "[%s] done\n", __func__ );
}

bool gc_should_collect( gbg_collector_t *gc ){
	gc->interval++;

	if ( gc->length > 20000 && gc->interval > 50000 ){
	//if ( gc->length > 100 && gc->interval > 500 ){
	//if ( true ){
		/*
		if ( gc->id > 1 ){
			printf( "[%s] Got here, %u\n", __func__, gc->id );
		}
		*/
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
	/*
	for ( i = 0; i < 3; i++ ){
		new_gc->colors[i].start  = NULL;
		new_gc->colors[i].end    = NULL;
		new_gc->colors[i].length = 0;
	}
	*/

	new_gc->start = new_gc->end = NULL;

	new_gc->interval = 0;
	new_gc->length = 0;

	/*
	new_gc->white = new_gc->black = new_gc->grey = NULL;
	new_gc->white_end = new_gc->black_end = new_gc->grey_end = NULL;
	new_gc->white_length = new_gc->black_length = new_gc->grey_length = 0;
	*/

	return new_gc;
}

gbg_collector_t *gc_merge( gbg_collector_t *first, gbg_collector_t *second ){
	unsigned i;

	/*
	for ( i = 0; i < 3; i++ ){
		gbg_list_t *a = &first->colors[i];
		gbg_list_t *b = &second->colors[i];

		if ( a->start ){
			if ( b->start ){
				b->start->prev = a->end;
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
	*/
		//printf( "[%s] Got here\n", __func__ );

	if ( first == second ){
		return first;
	}

	if ( first->start ){
		if ( second->start ){
			second->start->prev = first->end;
			first->end->next = second->start;
			first->end = second->end;
		}

		first->length += second->length;

	} else {
		first->start  = second->start;
		first->end    = second->end;
		first->length = second->length;
	}

	first->interval += second->interval;

	return first;
}

void gc_mark_envs( gbg_collector_t *garbage, env_t *top_env ){
	env_t *temp = top_env;

	for ( ; temp && temp->gc_link.status != GC_MARKED; temp = temp->last ){
	//for ( ; temp; temp = temp->last ){
		gc_mark_env( garbage, temp );
	}
}

void gc_mark_frames( gbg_collector_t *garbage, stack_frame_t *top_frame ){
	stack_frame_t *temp = top_frame;

	for ( ; temp; temp = temp->last ){
		//printf( "[%s] Marking frame %p, env: %p\n", __func__, temp, temp->env );

		if ( temp->env ){
			//gc_mark_env( garbage, temp->env );
			gc_mark_envs( garbage, temp->env );
		}

		gc_mark_tokens( garbage, temp->expr );
		gc_mark_tokens( garbage, temp->ptr );

		if ( temp->value ){
			gc_mark_tokens( garbage, temp->value );
		}

		if ( temp->cur_func ){
			gc_mark_tokens( garbage, temp->cur_func );
		}
	}
}

void gc_try_to_collect_frame( stack_frame_t *frame ){
	gbg_collector_t *garbage = get_current_gc( frame );

	if ( gc_should_collect( garbage )){
		gc_mark_frames( garbage, frame );
		//gc_collect( garbage, frame->value );
		//gc_collect( garbage, NULL );
		//dump_garbage_list( garbage );
		gc_collect( garbage );
	}
}

gbg_collector_t *get_current_gc( stack_frame_t *frame ){
	//return &frame->env->garbage;
	return frame->garbage;
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
