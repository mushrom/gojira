#include <gojira/runtime/garbage.h>
#include <gojira/tokens.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>

static inline gbg_node_t *gc_list_add( gbg_collector_t *gc, gbg_node_t *node, unsigned color ){
	gbg_list_t *list = &gc->colors[color];

	node->next = list->start;
	node->prev = NULL;
	node->status = color;

	list->start = node;
	list->length++;

	if ( node->next ){
		node->next->prev = node;
	}

	if ( !list->end ){
		list->end = node;
	}

	return node;
}

static inline gbg_node_t *gc_list_remove( gbg_collector_t *gc, gbg_node_t *node ){
	gbg_list_t *list = &gc->colors[node->status];

	if ( node == list->start ){
		list->start = node->next;
	}

	if ( node == list->end ){
		if ( node->prev ){
			list->end = node->prev;

		} else {
			list->end = list->start;
		}
	}

	if ( node->prev ){
		node->prev->next = node->next;
	}

	if ( node->next ){
		node->next->prev = node->prev;
	}

	node->prev = node->next = NULL;
	list->length--;

	return node;
}

static gbg_node_t *gc_list_move( gbg_collector_t *gc, gbg_node_t *node, unsigned color ){
	gbg_node_t *ret = node;

	if ( node->status < 3 ){
		gc_list_remove( gc, node );
		gc_list_add( gc, node, color );

	} else {
		printf( "[%s] Invalid move request\n", __func__ );
		printf( "\tToken color: %u, token: %p, next: %p, token gc: %u, gc id: %u\n",
			node->status, (void *)node, (void *)node->next, 1, gc->id );
	}

	return ret;
}

token_t *gc_alloc_token( gbg_collector_t *gc ){
	token_t *ret = gc_register( gc, alloc_token( ));

	return ret;
}

token_t *gc_clone_token( gbg_collector_t *gc, token_t *token ){
	token_t *ret = NULL;

	if ( token ){
		token_t *temp = clone_token( token );
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

	gc_list_add( gc, node, GC_COLOR_WHITE );

	return thing;
}

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
	//   - maybe have a dedicated 'condemned' list for objects freed manually,
	//     which then gets appended to the object cache first after GC cycles
}

#include <gojira/runtime/frame.h>
#include <gojira/runtime/runtime.h>
#include <gojira/libs/dlist.h>

void gc_move_upwards( gbg_collector_t *gc, void *thing, unsigned color ){
	if ( thing ){
		gbg_node_t *node = thing;

		if ( color > node->status ){
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

		gc_move_upwards( garbage, frame->last, GC_COLOR_GREY );
	}
}

void gc_mark_variable( gbg_collector_t *gc, variable_t *var ){
	if ( var ){
		gc_move_upwards( gc, var->token, GC_COLOR_GREY );
		gc_move_upwards( gc, var->left, GC_COLOR_GREY );
		gc_move_upwards( gc, var->right, GC_COLOR_GREY );
	}
}

void gc_set_interval( gbg_collector_t *gc, unsigned length, unsigned freed ){
	double ratio = (freed && length)? length / (freed * 1.0) : 1;
	double total = ratio - (gc->target_ratio - ratio);
	unsigned adjust = gc->interval * total;

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
				gc_mark_token( gc, (token_t *)foo );
				break;

			case GC_TYPE_ENVIRONMENT:
				gc_mark_env( gc, (env_t *)foo );
				break;

			case GC_TYPE_CONTINUATION:
				gc_mark_frame( gc, (stack_frame_t *)foo );
				break;

			case GC_TYPE_VARIABLE:
				gc_mark_variable( gc, (variable_t *)foo );
				break;

			case GC_TYPE_PROCEDURE:
				break;

			default:
				puts( "shouldn't get here..." );
				break;
		}

		foo = gc->colors[GC_COLOR_GREY].start;
	}

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

	if ( gc->iter >= gc->interval ){
		gc->iter = 0;
		return true;

	} else {
		return false;
	}
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

	new_gc->iter = 0;
	new_gc->interval = new_gc->default_interval;

	return new_gc;
}

/*
// Keeping this here in case it turns out to be useful in the future
gbg_collector_t *gc_merge( gbg_collector_t *first, gbg_collector_t *second ){
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

	return first;
}
*/

void gc_try_to_collect_frame( stack_frame_t *frame ){
	gbg_collector_t *garbage = get_current_gc( frame );

	if ( gc_should_collect( garbage )){
		gc_mark_frame( garbage, frame );
		gc_collect( garbage );
	}
}

gbg_collector_t *get_current_gc( stack_frame_t *frame ){
	return frame->garbage;
}
