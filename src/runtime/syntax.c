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

		/*
		foreach_in_dlist( i, dlst ){
			//printf( "[%s] Freeing vector element at %p\n", __func__, dlist_get( dlst, i ));
			//free_tokens( dlist_get( dlst, i ));
		}
		*/
		dlist_free( ptr );
	}
}

token_t *compile_lambda_body( stack_frame_t *frame, token_t *body ){
	token_t *ret = body;

	if ( ret ){
		if ( ret && ret->type == TYPE_SYMBOL ){
			const char *varname = shared_get( ret->data );

			token_t *vardata = env_find_var( frame->env, varname, RECURSE );

			if ( vardata && vardata->type == TYPE_SYNTAX ){
				vardata = gc_clone_token( get_current_gc( frame ), vardata );
				vardata->next = ret->next;

				ret = expand_syntax_rules( frame, vardata );
				if ( ret->type == TYPE_LIST ){
					ret = ret->down;
				}
			}
		}

		ret->down = compile_lambda_body( frame, ret->down );
		ret->next = compile_lambda_body( frame, ret->next );
	}

	return ret;
}

token_t *expand_lambda( stack_frame_t *frame, token_t *tokens ){
	token_t *ret = gc_alloc_token( get_current_gc( frame ));
	token_t *temp;
	shared_t *shr;

	procedure_t *proc = gc_register( get_current_gc( frame ), alloc_block_nozero( ));

	proc->gc_link.type = GC_TYPE_PROCEDURE;

	if ( tokens_length( tokens ) < 3 ){
		frame->error_call( frame,
				"[%s] Error: expected at least 3 tokens in lambda expression, but have %u\n",
				__func__, tokens_length( tokens ));
		return NULL;
	}

	temp = tokens->next->next;

	proc->env = frame->env;
	proc->args = tokens->next;
	proc->body = temp;
	proc->calls = 0;

	ret->type = TYPE_PROCEDURE;
	ret->proc = proc;

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

static void expand_procedure_args( stack_frame_t *frame, token_t *args, token_t *tokens ){
	if ( args->type == TYPE_LIST ){
		token_t *temp = args->down;
		token_t *move = tokens;

		foreach_in_list( temp ){
			if ( temp->type == TYPE_SYMBOL ){
				const char *var_name = shared_get( temp->data );

				// Handle variable-length arguments, which use the :rest keyword
				if ( strcmp( var_name, ":rest" ) == 0 ){
					if ( temp->next && temp->next->type == TYPE_SYMBOL ){
						token_t *newlist;

						var_name = shared_get( temp->next->data );
						newlist  = gc_alloc_token( get_current_gc( frame ));
						newlist->type = TYPE_LIST;
						newlist->down = move;

						env_add_var( frame->env, var_name, newlist, NO_RECURSE, VAR_MUTABLE );
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
						token_t *newtoken = gc_clone_token( get_current_gc( frame ), move );
						newtoken->next = NULL;
						env_add_var( frame->env, var_name, newtoken, NO_RECURSE, VAR_MUTABLE );
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

	} else if ( args->type == TYPE_SYMBOL ){
		const char *var_name = shared_get( args->data );
		token_t *temp = gc_alloc_token( get_current_gc( frame ));
		temp->type = TYPE_LIST;
		temp->down = tokens;
		env_add_var( frame->env, var_name, temp, NO_RECURSE, VAR_MUTABLE );

	} else {
		frame->error_call( frame,
		   "[%s] Error: invalid procedure definition, "
		   "expected parameter list or symbol but have %s\n",
		   __func__, type_str( args->type ));
	}
}

stack_frame_t *expand_procedure( stack_frame_t *frame, token_t *tokens ){
	stack_frame_t *ret = NULL;
	procedure_t *proc;
	token_t *temp;

	if ( tokens->type == TYPE_PROCEDURE ){
		proc = tokens->proc;

		if ( proc->calls == 3 ){
			token_t *temp = gc_register_tokens( get_current_gc( frame ), clone_tokens( proc->body ));
			proc->body = compile_lambda_body( frame, temp );
			proc->calls++;

		} else {
			proc->calls++;
		}

		frame->env = env_create( get_current_gc( frame ), proc->env );
		expand_procedure_args( frame, proc->args, tokens->next );

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
		if ( pattern->type == TYPE_SYMBOL ){

			if ( pattern->next ){
				if ( pattern->next->type == TYPE_SYMBOL ){
					char *next = shared_get( pattern->next->data );

					if ( strcmp(next, "...") == 0 )
						ret = true;

				} else {
					ret = false;
				}

			} else {
				char *str = shared_get( pattern->data );

				if ( strcmp(str, "...") == 0 )
					ret = true;
			}
		}
	}

	return ret;
}

variable_t *syntax_get_names( gbg_collector_t *gc, const token_t *pattern, token_t *args, variable_t *vars ){
	//hashmap_t *ret = map? map : hashmap_create(4);
	variable_t *ret = vars;
	variable_t *temp;

	if ( pattern ){
		switch ( pattern->type ){
			case TYPE_LIST:
			case TYPE_QUOTED_TOKEN:
				if ( args ){
					ret = syntax_get_names( gc, pattern->down, args->down, ret );
					ret = syntax_get_names( gc, pattern->next, args->next, ret );
				}
				break;

			case TYPE_SYMBOL: {
				char *name = shared_get( pattern->data );

				if ( pattern->next && pattern->next->type == TYPE_SYMBOL ){
					char *str = shared_get( pattern->next->data );

					if ( strcmp(str, "...") == 0 ){
						unsigned hash = hash_string( name );
						unsigned varhash = hash_string_accum( " #vararg", hash );
						//hashmap_add( ret, varhash, args );
						//hashmap_add( ret, hash, args );
						temp = variable_create( gc );
						temp->hash = hash;
						temp->token = args;
						ret = variable_insert( ret, temp );

						temp = variable_create( gc );
						temp->hash = varhash;
						temp->token = args;
						ret = variable_insert( ret, temp );
						DEBUGP( "[%s] Have variable-length pattern\n", __func__ );

					} else {
						if ( args ){
							DEBUGP( "[%s] Adding name \"%s\"\n", __func__, name );
							//hashmap_add( ret, hash_string( name ), args );
							temp = variable_create( gc );
							temp->hash = hash_string( name );
							temp->token = args;

							ret = variable_insert( ret, temp );

							ret = syntax_get_names( gc, pattern->next, args->next, ret );
						}
					}

				} else {
					if ( args ){
						//hashmap_add( ret, hash_string( name ), args );
						temp = variable_create( gc );
						temp->hash = hash_string( name );
						temp->token = args;

						ret = variable_insert( ret, temp );

						ret = syntax_get_names( gc, pattern->next, args->next, ret );
					}
				}

				break;
		    }

			default:
				if ( args ){
					ret = syntax_get_names( gc, pattern->next, args->next, ret );
				}
				break;
		}
	}

	return ret;
}

token_t *syntax_expand( gbg_collector_t *gbg, token_t *args, variable_t *vars ){
	token_t *ret = NULL;
	//token_t *temp;
	variable_t *tempvar;

	if ( args ){
		switch( args->type ){
			case TYPE_LIST:
				ret = gc_alloc_token( gbg );
				ret->type = TYPE_LIST;
				ret->down = syntax_expand( gbg, args->down, vars );
				ret->next = syntax_expand( gbg, args->next, vars );
				break;

			case TYPE_QUOTED_TOKEN:
				ret = gc_alloc_token( gbg );
				ret->type = TYPE_QUOTED_TOKEN;
				ret->down = syntax_expand( gbg, args->down, vars );
				ret->next = syntax_expand( gbg, args->next, vars );
				break;

			case TYPE_SYMBOL: {
				char *name = shared_get( args->data );

				if ( args->next && args->next->type == TYPE_SYMBOL ){
					char *str = shared_get( args->next->data );

					if ( strcmp(str, "...") == 0 ){
						unsigned hash = hash_string( name );
						hash = hash_string_accum( " #vararg", hash );

						//ret = gc_clone_token( gbg, hashmap_get( map, hash ));
						//ret = hashmap_get( map, hash );
						tempvar = variable_find( vars, hash );
						if ( tempvar ){
							ret = tempvar->token;
						}

					} else {
						//temp = hashmap_get( map, hash_string( name ));
						tempvar = variable_find( vars, hash_string( name ));

						ret = tempvar? gc_clone_token( gbg, tempvar->token )
						             : gc_clone_token( gbg, args );


						ret->next = syntax_expand( gbg, args->next, vars );
					}

				} else {
					//temp = hashmap_get( map, hash_string( name ));
					tempvar = variable_find( vars, hash_string( name ));

					ret = tempvar? gc_clone_token( gbg, tempvar->token )
					             : gc_clone_token( gbg, args );

					ret->next = syntax_expand( gbg, args->next, vars );
				}

				break;
		    }

			default:
				ret = gc_clone_token( gbg, args );
				ret->next = syntax_expand( gbg, args->next, vars );
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
				//hashmap_t *map;
				variable_t *vars;
				unsigned i;

				DEBUGP( "[%s] matched successfully\n", __func__ );

				matched = true;
				vars = syntax_get_names( get_current_gc( frame ), pattern, tokens, NULL );

				//printf( "got vars, %p\n", vars );

				//ret = gc_register_tokens( get_current_gc( frame ), syntax_expand( template, map ));
				ret = syntax_expand( get_current_gc( frame ), template, vars );

				// free the map's resources
				/*
				for ( i = 0; i < map->nbuckets; i++ ){
					list_free_nodes( map->buckets[i].base );
				}
				hashmap_free( map );
				*/

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
