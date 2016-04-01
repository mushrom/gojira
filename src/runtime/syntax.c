#include <gojira/runtime/syntax.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/shared.h>
#include <gojira/libs/dlist.h>
#include <gojira/runtime/garbage.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

bool has_symbol( token_t *tokens, char *sym ){
	bool ret = false;
	token_t *move;

	move = tokens;
	foreach_in_list( move ){
		if ( move->type == TYPE_SYMBOL && strcmp( shared_get( move->data ), sym ) == 0 ){
			ret = true;
			break;
		}
	}

	return ret;
}

void free_procedure( void *ptr ){
	if ( ptr ){
		procedure_t *proc = ptr;

		//printf( "[%s] Freeing procedure at %p\n", __func__, ptr );
		/*
		if ( proc->env ){
			env_release( proc->env );
		}
		*/

		//free_tokens( proc->body );
		//free_tokens( proc->args );
		free( proc );
	}
}

void free_vector( void *ptr ){
	if ( ptr ){
		dlist_t *dlst = ptr;
		unsigned i;

		//printf( "[%s] Freeing vector at %p\n", __func__, ptr );

		foreach_in_dlist( i, dlst ){
			//printf( "[%s] Freeing vector element at %p\n", __func__, dlist_get( dlst, i ));
			free_tokens( dlist_get( dlst, i ));
		}
	}
}

token_t *expand_lambda( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = gc_alloc_token( get_current_gc( frame ));
	token_t *temp;
	shared_t *shr;

	procedure_t *proc = calloc( 1, sizeof( procedure_t ));;

	if ( tokens_length( tokens ) < 3 ){
		frame->error_call( frame,
				"[%s] Error: expected at least 3 tokens in lambda expression, but have %u\n",
				__func__, tokens_length( tokens ));
		return NULL;
	}

	//temp = clone_tokens( tokens->next->next );
	//temp = gc_clone_token( get_current_gc( frame ), tokens->next->next );
	temp = tokens->next->next;

	//proc->env = frame->env? env_aquire( frame->env ) : NULL;
	proc->env = frame->env;

	//printf( "[%s] Storing environment at %p\n", __func__, proc->env );
	//proc->args = gc_clone_token( get_current_gc( frame ), tokens->next->down );
	proc->args = tokens->next->down;
		//clone_tokens( tokens->next->down );
	proc->body = temp;
	shr = shared_new( proc, free_procedure );

	ret->type = TYPE_PROCEDURE;
	ret->data = shr;
	ret->flags |= T_FLAG_HAS_SHARED;

	//ret->status = GC_MARKED;
	//gc_mark_tokens( get_current_gc( frame ), ret );

	return ret;
}

token_t *expand_vector( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = NULL;
	token_t *foo;

	if ( frame->ptr->down && frame->ptr->down->type == TYPE_LIST ){
		foo = alloc_token( );
		dlist_t *nlist = dlist_create( 0, 0 );
		shared_t *shr = shared_new( nlist, free_vector );
		token_t *temp = frame->ptr->down->down;

		foo->type = TYPE_VECTOR;
		foo->data = shr;
		foo->flags |= T_FLAG_HAS_SHARED;

		foreach_in_list( temp ){
			dlist_add( nlist, clone_token_tree( temp ));
		}

		ret = foo;

	} else {
		frame->error_call( frame, "[%s] Error: Invalid vector syntax\n", __func__ );
	}

	return ret;
}

stack_frame_t *expand_procedure( stack_frame_t *frame, token_t *tokens ){
	stack_frame_t *ret = NULL;
	token_t *move;
	token_t *temp;
	procedure_t *proc;
	char *var_name;
	shared_t *shr;

	if ( tokens->type == TYPE_PROCEDURE ){
		shr = tokens->data;
		proc = shared_get( shr );
		move = tokens->next;
		temp = proc->args;

		//gc_mark_env( &frame->gc, frame->env );
		//env_release( frame->env );
		//printf( "[%s] Old env was %p\n", __func__, frame->env );
		if ( frame->last ){
			//printf( "[%s] Previous frame env is %p\n", __func__, frame->last->env );
		}

		frame->env = env_create( get_current_gc( frame ), proc->env );
		//printf( "[%s] New env is %p\n", __func__, frame->env );
		frame->cur_func = gc_clone_token( get_current_gc( frame ), tokens );
		frame->cur_func->next = NULL;

		if ( frame->env->vars && frame->env->vars->nbuckets == 0 ){
			printf( "[%s} have no buckets at %p based on %p...\n",
				__func__, frame->env, proc->env );
		}

		foreach_in_list( temp ){
			if ( temp->type == TYPE_SYMBOL ){
				var_name = shared_get( temp->data );

				// Handle variable-length arguments, which use the :rest keyword
				if ( strcmp( var_name, ":rest" ) == 0 ){
					if ( temp->next && temp->next->type == TYPE_SYMBOL ){
						token_t *newlist;

						var_name = shared_get( temp->next->data );
						newlist  = gc_alloc_token( get_current_gc( frame ));
						newlist->type = TYPE_LIST;
						newlist->down = move;

						//frame_add_var( frame, var_name, newlist, NO_RECURSE, VAR_IMMUTABLE );
						env_add_var( frame->env, var_name, newlist, NO_RECURSE, VAR_IMMUTABLE );

						//free_token( newlist );
						break;

					} else {
						frame->error_call( frame,
							"[%s] Error: Have unbound variable after :rest\n",
							__func__ );
						break;
					}

				// otherwise handle regular variable bindings
				} else {
					if ( move ){
						// TODO: allow specifying mutable parameters
						//frame_add_var( frame, var_name, move, NO_RECURSE, VAR_IMMUTABLE );
						token_t *newtoken = gc_clone_token( get_current_gc( frame ), move );
						newtoken->next = NULL;
						env_add_var( frame->env, var_name, newtoken, NO_RECURSE, VAR_IMMUTABLE );
						move = move->next;

					} else {
						frame->error_call( frame,
							"[%s] Error: Have unbound variable \"%s\"\n",
							__func__, var_name );
						break;
					}
				}

			} else {
				frame->error_call( frame,
					"[%s] Error: expected symbol in procedure definition, have \"%s\"\n",
					__func__, type_str( temp->type ));
			}
		}

		ret = frame;
		ret->expr = ret->end = NULL;

		temp = ext_proc_token( builtin_return_last );
		gc_register( get_current_gc( ret ), temp );
		frame_add_token_noclone( ret, temp );

		ret->ptr = proc->body;

	} else {
		frame->error_call( frame,
			"[%s] Error: Trying to apply non-procedure as procedure (?!)\n",
			__func__ );
	}

	return ret;
}

bool syntax_matches( const token_t *pattern, const token_t *args ){
	bool ret = (pattern && args) || (!pattern && !args);

	if ( pattern && args ){
		switch ( pattern->type ){
			case TYPE_LIST:
				ret = args->type == TYPE_LIST && syntax_matches( pattern->down, args->down );
				ret = ret && syntax_matches( pattern->next, args->next );
				break;

			case TYPE_SYMBOL:
				if ( pattern->next && pattern->next->type == TYPE_SYMBOL ){
					char *str = shared_get( pattern->next->data );
					if ( strcmp(str, "...") == 0 ){
						DEBUGP( "[%s] Have variable-length pattern\n", __func__ );
						ret = true;

					} else {
						ret = syntax_matches( pattern->next, args->next );
					}

				} else {
					ret = syntax_matches( pattern->next, args->next );
				}

				break;

			default:
				DEBUGP( "[%s] Matching a literal value, %d==%d, %d\n",
						__func__, pattern->smalldata, args->smalldata, ret );
				ret =  ( pattern->type      == args->type )
				    //&& ( pattern->smalldata == args->smalldata )
					// TODO: do this properly
				    && ( pattern->character == args->character )
				    && syntax_matches( pattern->next, args->next );
				break;
		}

	} else if ( pattern ){
		if ( pattern->type == TYPE_STRING ){
			char *str = shared_get( pattern->data );

			if ( strcmp(str, "...") == 0 )
				ret = true;
		}
	}

	return ret;
}

hashmap_t *syntax_get_names( const token_t *pattern, token_t *args, hashmap_t *map ){
	hashmap_t *ret = map? map : hashmap_create(4);

	if ( pattern && args ){
		switch ( pattern->type ){
			case TYPE_LIST:
			case TYPE_QUOTED_TOKEN:
				ret = syntax_get_names( pattern->down, args->down, ret );
				ret = syntax_get_names( pattern->next, args->next, ret );
				break;

			case TYPE_SYMBOL: {
				char *name = shared_get( pattern->data );

				if ( pattern->next && pattern->next->type == TYPE_SYMBOL ){
					char *str = shared_get( pattern->next->data );

					if ( strcmp(str, "...") == 0 ){
						unsigned hash = hash_string( name );
						unsigned varhash = hash_string_accum( " #vararg", hash );
						hashmap_add( ret, varhash, args );
						hashmap_add( ret, hash, args );
						DEBUGP( "[%s] Have variable-length pattern\n", __func__ );

					} else {
						DEBUGP( "[%s] Adding name \"%s\"\n", __func__, name );
						hashmap_add( ret, hash_string( name ), args );
						ret = syntax_get_names( pattern->next, args->next, ret );
					}

				} else {
					hashmap_add( ret, hash_string( name ), args );
					ret = syntax_get_names( pattern->next, args->next, ret );
				}

				break;
		    }

			default:
				ret = syntax_get_names( pattern->next, args->next, ret );
				break;
		}
	}

	return ret;
}

token_t *syntax_expand( token_t *args, hashmap_t *map ){
	token_t *ret = NULL;
	token_t *temp;

	if ( args ){
		switch( args->type ){
			case TYPE_LIST:
				ret = alloc_token( );
				ret->type = TYPE_LIST;
				ret->down = syntax_expand( args->down, map );
				ret->next = syntax_expand( args->next, map );
				break;

			case TYPE_QUOTED_TOKEN:
				ret = alloc_token( );
				ret->type = TYPE_QUOTED_TOKEN;
				ret->down = syntax_expand( args->down, map );
				ret->next = syntax_expand( args->next, map );
				break;

			case TYPE_SYMBOL: {
				char *name = shared_get( args->data );

				if ( args->next && args->next->type == TYPE_SYMBOL ){
					char *str = shared_get( args->next->data );

					if ( strcmp(str, "...") == 0 ){
						unsigned hash = hash_string( name );
						hash = hash_string_accum( " #vararg", hash );

						ret = clone_tokens( hashmap_get( map, hash ));

					} else {
						temp = hashmap_get( map, hash_string( name ));

						ret = temp? clone_token_tree( temp )
						          : clone_token( args );

						ret->next = syntax_expand( args->next, map );
					}

				} else {
					temp = hashmap_get( map, hash_string( name ));

					ret = temp? clone_token_tree( temp )
					          : clone_token( args );

					ret->next = syntax_expand( args->next, map );
				}

				break;
		    }

			default:
				ret = clone_token_tree( args );
				ret->next = syntax_expand( args->next, map );
				break;
		}
	}

	return ret;
}

token_t *expand_syntax_rules( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = NULL;

	token_t *cur;
	token_t *pattern;
	token_t *template;

	bool matched = false;
	int len;

	cur = tokens->down;
	len = tokens_length( cur );

	if ( len >= 3 ){
		cur = cur->next->next;

		for ( ; cur; cur = cur->next ){
			pattern = cur->down->down;
			template = cur->down->next;

			if ( syntax_matches( pattern, tokens )){
				hashmap_t *map;
				unsigned i;

				DEBUGP( "[%s] matched successfully\n", __func__ );

				matched = true;
				map = syntax_get_names( pattern, tokens, NULL );
				ret = gc_register_tokens( get_current_gc( frame ), syntax_expand( template, map ));

				// free the map's resources
				for ( i = 0; i < map->nbuckets; i++ ){
					list_free_nodes( map->buckets[i].base );
				}
				hashmap_free( map );

				/*
				gc_unmark( ret );
				frame_register_token_tree( frame, ret );
				*/
				//gc_register_token_tree( &frame->gc, ret );
				break;

			} else {
				DEBUGP( "[%s] Didn't match pattern, continuing...\n", __func__ );
			}
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected at least 3 tokens, but got %d\n", __func__, len );
	}

	if ( !matched ){
		frame->error_call( frame, "[%s] Error: Could not match syntax pattern\n", __func__ );
	}

	return ret;
}
