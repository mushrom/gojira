#include <gojira/runtime/frame.h>
#include <gojira/config.h>

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

/*
env_t *env_aquire( env_t *env ){
	if ( env ){
		env->refs++;
	}

	return env;
}
*/

/*
void env_release( env_t *env ){
	if ( env ){
		if ( env->refs == 0 ){
			printf( "[%s] Warning, freeing an already freed environment!\n", __func__ );
		}

		env->refs--;

		if ( env->refs == 0 ){
			env_free_vars( env );

			env_release( env->last );
			free( env );
			//printf( "[%s] Got here, %p\n", __func__, env );
		}
	}
}
*/

void env_free( env_t *env ){
	env_free_vars( env );

	free( env );
	//printf( "[%s] Got here, %p\n", __func__, env );
}

env_t *env_create( gbg_collector_t *gc, env_t *last ){
	//env_t *ret = calloc( 1, sizeof( env_t ));
	env_t *ret = gc_register( gc, calloc( 1, sizeof( env_t )));
	//ret->last = env_aquire( last );
	ret->last = last;
	ret->gc_link.type = GC_TYPE_ENVIRONMENT;
	//ret->refs = 1;

	/*
	if ( last ){
		gc_init( &last->garbage, &ret->garbage );

	} else {
		gc_init( NULL, &ret->garbage );
	}
	*/
	//printf( "[%s] Got here, %p -> %p\n", __func__, ret, ret->last );
	//printf( "[%s] Got here\n", __func__ );

	return ret;
}

env_t *env_free_vars( env_t *env ){
	list_node_t *move, *temp;
	hashmap_t *map;
	unsigned i;

	if ( env ){
		if ( env->vars ){
			map = env->vars;
			for ( i = 0; i < map->nbuckets; i++ ){
				move = map->buckets[i].base;

				for ( ; move; move = temp ){
					temp = move->next;
					shared_release( move->data );
					free( move );
				}
			}

			hashmap_free( env->vars );
			env->vars = NULL;
		}
	}

	return NULL;
}

//variable_t *frame_find_var_struct_hash( st_frame_t *frame, unsigned hash, bool recurse ){
variable_t *env_find_var_struct_hash( env_t *env, unsigned hash, bool recurse ){
	variable_t *ret = NULL;
	shared_t *shr;

	if ( env ){
		if ( env->vars ){
			shr = hashmap_get( env->vars, hash );
			if ( shr ){
				ret = shared_get( shr );
			}

			if ( !ret && recurse ){
				ret = env_find_var_struct_hash( env->last, hash, recurse );
			}

		} else if ( recurse ){
			ret = env_find_var_struct_hash( env->last, hash, recurse );
		}
	}

	return ret;
}

shared_t *env_find_shared_struct_hash( env_t *env, unsigned hash, bool recurse ){
	shared_t *ret = NULL;

	if ( env ){
		if ( env->vars ){
			ret = hashmap_get( env->vars, hash );

			if ( !ret && recurse ){
				ret = env_find_shared_struct_hash( env->last, hash, recurse );
			}

		} else if ( recurse ){
			ret = env_find_shared_struct_hash( env->last, hash, recurse );
		}
	}

	return ret;
}

token_t *env_find_var_hash( env_t *env, unsigned hash, bool recurse ){
	token_t *ret = NULL;
	variable_t *var;

	var = env_find_var_struct_hash( env, hash, recurse );
	if ( var ){
		ret = var->token;
	}

	return ret;
}

token_t *env_find_var( env_t *env, const char *key, bool recurse ){
	token_t *ret = NULL;
	unsigned hash;

	hash = hash_string( key );
	ret = env_find_var_hash( env, hash, recurse );

	return ret;
}

variable_t *env_find_var_struct( env_t *env, const char *key, bool recurse ){
	variable_t *ret = NULL;
	unsigned hash;

	hash = hash_string( key );
	ret = env_find_var_struct_hash( env, hash, recurse );

	return ret;
}

shared_t *env_find_shared_struct( env_t *env, const char *key, bool recurse ){
	shared_t *ret = NULL;
	unsigned hash;

	hash = hash_string( key );
	ret = env_find_shared_struct_hash( env, hash, recurse );

	return ret;
}

void free_var( void *ptr ){
	if ( ptr ){
		variable_t *var = ptr;
		//printf( "[%s] Freeing variable with hash 0x%x\n", __func__, var->hash );
		//free_tokens( var->token );
		free( var->key );
		free( var );
	}
}

variable_t *env_add_var( env_t *env, const char *key, token_t *token, bool recurse, bool mutable ){
	variable_t *new_var = NULL;
	shared_t *new_shared = NULL;

	if ( env ){
		if ( !env->vars )
			env->vars = hashmap_create( 8 );

		new_var = env_find_var_struct( env, key, recurse );

		if ( !new_var ){
			new_var = calloc( 1, sizeof( variable_t ));
			new_var->key = strdup( key );
			new_var->hash = hash_string( key );
			new_var->is_mutable = mutable;
			//new_var->token = clone_token_tree( token );
			new_var->token = token;
			new_shared = shared_new( new_var, free_var );

			hashmap_add( env->vars, new_var->hash, new_shared );

		} else if ( new_var->is_mutable ){
			new_var->token = clone_token_tree( token );

		} else {
			/* TODO: existing variable isn't mutable, so error out */
			printf( "[%s] Error: variable \"%s\" is not mutable\n",
					__func__, key );

			//stack_trace( env );
		}

	} else {
		printf( "[%s] Warning: Got null env, can't add variable \"%s\"\n",
				__func__, key );
	}

	return new_var;
}

