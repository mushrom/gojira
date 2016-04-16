#include <gojira/runtime/garbage.h>
#include <gojira/tokens.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

static gbg_node_t *gc_list_add( gbg_collector_t *gc, gbg_node_t *node, unsigned color ){
	gbg_list_t *list = &gc->colors[color];

	if ( !gc ){
		printf( "[%s] Given a null GC, things will break\n", __func__ );
	}

	if ( list->start && !list->end ){
		printf( "[%s] Have a list with a start but no end\n", __func__ );
	}

	//node->id = gc->id;
	node->next = list->start;
	node->prev = NULL;
	//node->gc_data = gc;
	node->status = color;

	//list->start = node;
	list->start = node;
	list->length++;

	if ( node->next ){
		node->next->prev = node;
	}

	if ( !list->end ){
		list->end = node;
	}

	// TODO: remove these comments eventually,
	//       these are here in case they're needed for debugging
	/*
	if ( list->start->prev != NULL ){
		printf( "[%s] Have a start with nodes after it...\n", __func__ );
	}

	if ( list->end->next != NULL ){
		printf( "[%s] Have an end with nodes after it...\n", __func__ );
		//for ( ; gc->end->gc_next; gc->end = gc->end->gc_next );
	}

	if ( node->next == node ){
		printf( "[%s] node %p->gc_next is itself???\n", __func__, node );
	}

	if ( node->prev == node ){
		printf( "[%s] node %p->gc_prev is itself???\n", __func__, node );
	}
	*/

	return node;
}

static gbg_node_t *gc_list_remove( gbg_collector_t *gc, gbg_node_t *node ){
	gbg_list_t *list = &gc->colors[node->status];

	//printf( "[%s] removing %p, %u, start = %p, end = %p\n", __func__, node, gc->id, list->start, list->end );

	/*
	if ((!gc->start && !gc->end) || node->status >= GC_MARKED || gc->length == 0 ){
		printf( "[%s] Got bad list remove request, node: %p, color: %u\n",
			__func__, node, node->status );

		printf( "\tnode color: %u, node: %p, prev: %p, next: %p, node gc: %u, gc id: %u\n",
			__func__, node->status, node, node->prev, node->prev,
			node->id, gc->id );
		return node;
	}
	*/

	/*
	if ( node->next == node ){
		printf( "[%s] node %p->next is itself???\n", __func__, node );
	}

	if ( node->prev == node ){
		printf( "[%s] node %p->prev is itself???\n", __func__, node );
	}
	*/

	if ( node == list->start ){
		list->start = node->next;
		//printf( "[%s] Set new list start, %p\n", __func__, list->start );
	}

	if ( node == list->end ){
		if ( node->prev ){
			list->end = node->prev;
		} else {
			list->end = list->start;
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

		/*
		if ( node->prev->next != node ){
			printf( "\tPrevious node %p in color list does not point to %p, actually points to %p...\n",
					node->prev, node, node->prev->next );
		}
		*/

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

		/*
		if ( node->next->prev != node ){
			printf( "\tNext node %p in color list does not point to %p, actually points to %p... %d\n",
				node->next, node, node->next->prev, node != node->next->prev );
		}
		*/

		//printf( "[%s] Set new %p->prev to %p\n", __func__, node->next, node->prev );

		node->next->prev = node->prev;
	}

	node->prev = node->next = NULL;

	list->length--;

	return node;
}

static gbg_node_t *gc_list_move( gbg_collector_t *gc, gbg_node_t *node, unsigned color ){
	gbg_node_t *ret = node;

	if ( node->status < 3 /* && gc->id >= gc->id */ ){
		gc_list_remove( gc, node );
		gc_list_add( gc, node, color );

	} else {
		printf( "[%s] Invalid move request\n", __func__ );
		printf( "\tToken color: %u, token: %p, next: %p, token gc: %u, gc id: %u\n",
			__func__, node->status, node, node->next, /*node->id*/ 1, gc->id );
	}

	return ret;
}

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

token_t *gc_clone_token_spine( gbg_collector_t *gc, token_t *token ){
	token_t *ret = NULL;

	if ( token ){
		ret = gc_clone_token( gc, token );

		if ( token->next ){
			ret->next = gc_clone_token_spine( gc, token->next );
		}
	}

	return ret;
}

#include <signal.h>

void *gc_register( gbg_collector_t *gc, void *thing ){
	gbg_node_t *node = thing;

	//node->status = GC_UNMARKED;

	gc_list_add( gc, node, GC_COLOR_WHITE );

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
#include <gojira/libs/dlist.h>

void gc_move_upwards( gbg_collector_t *gc, void *thing, unsigned color ){
	if ( thing ){
		gbg_node_t *node = thing;

		if ( node->status == GC_COLOR_WHITE || color > node->status ){
			gc_list_move( gc, node, color );
		}
	}
}

void gc_mark_env( gbg_collector_t *gc, env_t *env );
void gc_mark_hashmap( gbg_collector_t *gc, hashmap_t *map );
void gc_mark_vector( gbg_collector_t *gc, dlist_t *dlst );
void gc_mark_frame( gbg_collector_t *garbage, stack_frame_t *top_frame );
void gc_mark_variable( gbg_collector_t *garbage, variable_t *var );

void gc_mark_token( gbg_collector_t *gc, token_t *token ){
	if ( token ){
		gc_move_upwards( gc, token, GC_COLOR_GREY );

		if ( token->type == TYPE_PROCEDURE ){
			//printf( "[%s] Got here\n", __func__ );
			//procedure_t *proc = shared_get( token->data );
			procedure_t *proc = token->proc;

			gc_move_upwards( gc, proc, GC_COLOR_GREY );
			gc_move_upwards( gc, proc->env, GC_COLOR_GREY );
			gc_move_upwards( gc, proc->body, GC_COLOR_GREY );
			gc_move_upwards( gc, proc->args, GC_COLOR_GREY );
		}

		if ( token->type == TYPE_HASHMAP ){
			hashmap_t *map = shared_get( token->data );

			gc_mark_hashmap( gc, map );
		}

		if ( token->type == TYPE_VECTOR ){
			dlist_t *dlst = shared_get( token->data );

			gc_mark_vector( gc, dlst );
		}

		if ( token->type == TYPE_CONTINUATION ){
			gc_move_upwards( gc, token->cont, GC_COLOR_GREY );
		}

		gc_move_upwards( gc, token->down, GC_COLOR_GREY );
		gc_move_upwards( gc, token->next, GC_COLOR_GREY );
	}
}

void gc_mark_hashmap( gbg_collector_t *gc, hashmap_t *map ){
	list_node_t *node;
	list_node_t *temp;
	unsigned i;

	for ( i = 0; i < map->nbuckets; i++ ){
		node = map->buckets[i].base;

		for ( ; node; node = temp ){
			temp = node->next;
			//gc_mark_token( gc, node->data );
			gc_move_upwards( gc, node->data, GC_COLOR_GREY );
		}
	}
}

void gc_mark_vector( gbg_collector_t *gc, dlist_t *dlst ){
	unsigned i;

	foreach_in_dlist( i, dlst ){
		gc_move_upwards( gc, dlist_get( dlst, i ), GC_COLOR_GREY );
	}
}

void gc_mark_env( gbg_collector_t *gc, env_t *env ){
	if ( env ){
		gc_move_upwards( gc, env, GC_COLOR_GREY );
		gc_move_upwards( gc, env->last, GC_COLOR_GREY );
		gc_move_upwards( gc, env->vars, GC_COLOR_GREY );

		/*
		if ( env->vars ){
			unsigned i;

			hashmap_t *map = env->vars;

			for ( i = 0; i < map->nbuckets; i++ ){
				list_node_t *node = map->buckets[i].base;

				foreach_in_list( node ){
					variable_t *var = shared_get( node->data );
					gc_move_upwards( gc, var->token, GC_COLOR_GREY );
				}
			}
		}
		*/

	}
}

void gc_mark_frame( gbg_collector_t *garbage, stack_frame_t *frame ){
	if ( frame ){
		gc_move_upwards( garbage, frame, GC_COLOR_GREY );

		if ( frame->env ){
			gc_move_upwards( garbage, frame->env, GC_COLOR_GREY );
		}

		gc_move_upwards( garbage, frame->expr, GC_COLOR_GREY );
		gc_move_upwards( garbage, frame->ptr, GC_COLOR_GREY );

		if ( frame->value ){
			gc_move_upwards( garbage, frame->value, GC_COLOR_GREY );
		}

		/*
		if ( frame->cur_func ){
			//gc_mark_tokens( garbage, temp->cur_func );
		}
		*/

		gc_move_upwards( garbage, frame->last, GC_COLOR_GREY );
	}
}

void gc_mark_variable( gbg_collector_t *gc, variable_t *var ){
	if ( var ){
		gc_move_upwards( gc, var->token, GC_COLOR_GREY );
		//gc_move_upwards( gc, var->next, GC_COLOR_GREY );
		gc_move_upwards( gc, var->left, GC_COLOR_GREY );
		gc_move_upwards( gc, var->right, GC_COLOR_GREY );
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

		case GC_TYPE_CONTINUATION:
			frame_free( (stack_frame_t *)node );
			break;

		default:
			printf( "have unknown type %u?\n", node->type );
			break;
	}
}

void gc_set_interval( gbg_collector_t *gc, unsigned length, unsigned freed ){
	//double target = 1.15;
	double ratio = (freed && length)? length / (freed * 1.0) : 1;
	double total = ratio - (gc->target_ratio - ratio);
	//ratio -= 0.05;
	//unsigned adjust = gc->interval * ratio;
	//unsigned adjust = gc->interval + gc->interval * (diff * 5 - 0.075);
	unsigned adjust = gc->interval * total;

	/*
	printf( "[set_interval] interval: %u, length: %u, freed: %u, remaining: %u, "
			"ratio: %f, total: %f, adjust: %u\n",
		gc->interval, length, freed, length - freed, ratio, total, adjust );
	 */

	gc->interval = adjust;
}

#include <gojira/runtime/allocate.h>

void gc_collect( gbg_collector_t *gc ){
	gbg_node_t *temp;
	gbg_node_t *foo;

	foo = gc->colors[GC_COLOR_GREY].start;

	unsigned length = gc->colors[GC_COLOR_WHITE].length + gc->colors[GC_COLOR_GREY].length;
	unsigned freed = 0;

	while ( foo ){
		gc_move_upwards( gc, foo, GC_COLOR_BLACK );

		switch ( foo->type ){
			case GC_TYPE_TOKEN:
				//puts( "marking token..." );
				gc_mark_token( gc, (token_t *)foo );
				break;

			case GC_TYPE_ENVIRONMENT:
				//puts( "marking environment..." );
				gc_mark_env( gc, (env_t *)foo );
				break;

			case GC_TYPE_CONTINUATION:
				//puts( "marking continuation..." );
				gc_mark_frame( gc, (stack_frame_t *)foo );
				break;

			case GC_TYPE_VARIABLE:
				gc_mark_variable( gc, (variable_t *)foo );
				break;

			default:
				puts( "shouldn't get here..." );
				break;
		}

		foo = gc->colors[GC_COLOR_GREY].start;
	}

	/*
	while (( temp = gc->colors[GC_COLOR_WHITE].start )){
		gc_list_remove( gc, temp );
		free_gbg_node( temp );
	}
	*/
	cache_blocks( &gc->colors[GC_COLOR_WHITE] );

	{
		gbg_list_t *list = &gc->colors[GC_COLOR_WHITE];

		list->start = list->end = NULL;
		list->length = 0;
	}

	while (( temp = gc->colors[GC_COLOR_BLACK].start )){
		gc_list_move( gc, temp, GC_COLOR_WHITE );
	}


	freed = length - gc->colors[GC_COLOR_WHITE].length;
	gc_set_interval( gc, length, freed );
}

bool gc_should_collect( gbg_collector_t *gc ){
	gc->iter++;

	//if ( gc->length > 20000 && gc->iter > 50000 ){
	if ( gc->iter >= gc->interval ){
		gc->iter = 0;
		return true;

	} else {
		return false;
	}

	//return gc->colors[GC_COLOR_WHITE].length > 50;
}

void gc_set_profile( gbg_collector_t *gbg, unsigned profile ){
	switch ( profile ){
		case GC_PROFILE_FAST:
			gbg->default_interval = 50000;
			gbg->target_ratio = 1.02;
			break;

		case GC_PROFILE_BALANCED:
			gbg->default_interval = 10000;
			gbg->target_ratio = 1.15;
			break;

		case GC_PROFILE_LOWMEM:
			gbg->default_interval = 100;
			gbg->target_ratio = 1.70;
			break;

		default:
			// Unknown profile?
			break;
	}

	gbg->interval = gbg->default_interval;
}

gbg_collector_t *gc_init( gbg_collector_t *old_gc, gbg_collector_t *new_gc ){
	if ( old_gc ){
		new_gc->id = old_gc->id + 1;
		new_gc->default_interval = old_gc->default_interval;
		new_gc->target_ratio = old_gc->target_ratio;

	} else {
		gc_set_profile( new_gc, GC_PROFILE_BALANCED );
		new_gc->id = 1;
	}

	unsigned i;
	for ( i = 0; i < 3; i++ ){
		new_gc->colors[i].start  = NULL;
		new_gc->colors[i].end    = NULL;
		new_gc->colors[i].length = 0;
	}

	//new_gc->start = new_gc->end = NULL;

	new_gc->iter = 0;
	new_gc->interval = new_gc->default_interval;
	//new_gc->length = 0;

	/*
	new_gc->white = new_gc->black = new_gc->grey = NULL;
	new_gc->white_end = new_gc->black_end = new_gc->grey_end = NULL;
	new_gc->white_length = new_gc->black_length = new_gc->grey_length = 0;
	*/

	return new_gc;
}

gbg_collector_t *gc_merge( gbg_collector_t *first, gbg_collector_t *second ){
	/*
	unsigned i;

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

	/*
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

	first->iter += second->iter;
	*/

	return first;
}

/*
void gc_mark_envs( gbg_collector_t *garbage, env_t *top_env ){
	env_t *temp = top_env;
	unsigned i;

	//printf( "[%s] marking envs, %p...\n", __func__, temp );

	for ( i = 0; temp && temp->gc_link.status != GC_MARKED; temp = temp->last ){
	//for ( ; temp; temp = temp->last ){
		gc_mark_env( garbage, temp );
		i++;
	}

	//printf( "[%s] marked %u environments at %p...\n", __func__, i, top_env );
}
*/


void gc_try_to_collect_frame( stack_frame_t *frame ){
	gbg_collector_t *garbage = get_current_gc( frame );

	if ( gc_should_collect( garbage )){
		//printf( "[%s] starting garbage collection...\n", __func__ );
		//printf( "[%s] marking things...\n", __func__ );
		gc_mark_frame( garbage, frame );
		//gc_collect( garbage, frame->value );
		//gc_collect( garbage, NULL );
		//dump_garbage_list( garbage );
		//printf( "[%s] trying to collect %u values...\n", __func__, garbage->length );
		gc_collect( garbage );
		//printf( "[%s] done\n", __func__ );
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
