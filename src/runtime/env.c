#include <gojira/runtime/frame.h>
#include <gojira/config.h>

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

inline variable_t *variable_create( gbg_collector_t *gc ){
	variable_t *ret = gc_register( gc, alloc_block( ));

	ret->gc_link.type = GC_TYPE_VARIABLE;

	return ret;
}

variable_t *variable_insert( variable_t *tree, variable_t *value ){
	variable_t *ret = value;

	if ( tree ){
		if ( value->hash < tree->hash ){
			tree->left = variable_insert( tree->left, value );

		} else {
			tree->right = variable_insert( tree->right, value );
		}

		ret = tree;
	}

	return ret;
}

variable_t *variable_find( variable_t *tree, unsigned hash ){
	variable_t *ret = NULL;

	if ( tree ){
		if ( hash == tree->hash ){
			ret = tree;

		} else if ( hash < tree->hash ){
			ret = variable_find( tree->left, hash );

		} else {
			ret = variable_find( tree->right, hash );
		}
	}

	return ret;
}

void env_free( env_t *env ){
	free( env );
}

env_t *env_create( gbg_collector_t *gc, env_t *last ){
	env_t *ret = gc_register( gc, alloc_block ( ));
	ret->last = last;
	ret->gc_link.type = GC_TYPE_ENVIRONMENT;
	ret->garbage = gc;

	return ret;
}

variable_t *env_find_var_struct_hash( env_t *env, unsigned hash, bool recurse ){
	variable_t *ret = NULL;

	if ( env ){
		if ( env->vars ){
			ret = variable_find( env->vars, hash );

			if ( !ret && recurse ){
				ret = env_find_var_struct_hash( env->last, hash, recurse );
			}

		} else if ( recurse ){
			ret = env_find_var_struct_hash( env->last, hash, recurse );
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

void free_var( void *ptr ){
	if ( ptr ){
		variable_t *var = ptr;
		free( var->key );
		free( var );
	}
}

variable_t *env_add_var( env_t *env, const char *key, token_t *token, bool recurse, unsigned mutable ){
	variable_t *new_var = NULL;

	if ( env ){
		new_var = env_find_var_struct( env, key, recurse );

		if ( !new_var ){
			new_var = variable_create( env->garbage );
			new_var->key = "<tab complete borked, TODO: fix tab completion>";
			new_var->hash = hash_string( key );
			new_var->is_mutable = mutable;
			new_var->token = token;

			env->vars = variable_insert( env->vars, new_var );

		} else if ( new_var->is_mutable ){
			if ( new_var->is_mutable == VAR_MUTABLE_BUILTIN ){
				// change variable to 'mutable' after changing the variable to prevent
				// spamming output with warning messages
				new_var->is_mutable = VAR_MUTABLE;

				printf( "[%s] Warning: changing built-in \"%s\"\n",
					__func__, key );
			}

			new_var->token = token;

		} else {
			/* TODO: existing variable isn't mutable, so error out */
			printf( "[%s] Error: variable \"%s\" is not mutable\n",
					__func__, key );
		}

	} else {
		printf( "[%s] Warning: Got null env, can't add variable \"%s\"\n",
				__func__, key );
	}

	return new_var;
}

