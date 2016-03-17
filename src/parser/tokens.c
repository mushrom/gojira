#include <stdio.h>
#include <stdlib.h>
#include <gojira/parse_debug.h>
#include <gojira/parser.h>
#include <gojira/lexer.h>
#include <gojira/tokens.h>
#include <stdbool.h>
#include <string.h>

#include <gojira/libs/shared.h>
#include <gojira/libs/dlist.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>

void debug_print_token( const token_t *token ){
	if ( token ){
		switch ( token->type ){
			case TYPE_NUMBER:
				printf( "%ld", token->number.s_int );
				break;

			case TYPE_BOOLEAN:
				printf( "#%c", (token->boolean == true)? 't' : 'f' );
				break;

			case TYPE_CHAR:
				printf( "%c", token->character );
				break;

			case TYPE_STRING:
			case TYPE_SYMBOL:
				printf( "%s", (char *)shared_get( token->data ));
				break;

			default:
				printf( "#<%s>", type_str( token->type ));
				break;
		}
	}
}

const token_t *debug_print_iter( const token_t *tokens, unsigned level ){
	if ( tokens ){
		unsigned i;
		/*
		if ( tokens->status == GC_MARKED ){
			printf( "[marked]       " );
		} else if ( tokens->status == GC_UNMARKED ){
			printf( "[unmarked]     " );
		} else if ( tokens->status == GC_FREED ){
			printf( "[freed]        " );
		} else {
			printf( "[unknown (%d)] ", tokens->status );
		}
		*/

		for ( i = 0; i < level; i++ )
			printf( "    " );

		debug_print_token( tokens );
		printf( "\n" );

		debug_print_iter( tokens->down, level + 1 );
		debug_print_iter( tokens->next, level );
	}

	return tokens;
}

const token_t *debug_print( const token_t *tokens ){
	return debug_print_iter( tokens, 0 );
}

// Strips a token from a tree. If the stripped token has a lower token,
// the lower token (and all "next" nodes) will be merged into the same level
// as the original token.
token_t *strip_token( token_t *tokens, type_t type ){
	token_t *ret = tokens;
	token_t *temp;

	if ( tokens ){
		if ( tokens->type == type ){
			if ( tokens->down ){
				ret = strip_token( tokens->down, type );
				temp = ret->next;
				if ( temp ){
					while ( temp->next ) temp = temp->next;
					temp->next = strip_token( tokens->next, type );
				} else {
					ret->next = strip_token( tokens->next, type );
				}

				free( tokens );

			} else {
				ret = strip_token( tokens->next, type );
				free( tokens );
			}

		} else {
			ret->down = strip_token( tokens->down, type );
			ret->next = strip_token( tokens->next, type );
		}
	}

	return ret;
}

token_t *remove_token_list( token_t *tokens, type_t remove[], unsigned n ){
	token_t *ret = tokens;
	int i;

	for ( i = 0; ret && i < n; i++ )
		ret = strip_token( ret, remove[i] );

	return ret;
}

// Removes all "punctuation" tokens from a tree, which are unneeded after parsing is done
token_t *remove_punc_tokens( token_t *tokens ){
	token_t *ret = tokens;
	type_t remove[] = {
		TYPE_OPEN_PAREN, TYPE_CLOSE_PAREN, TYPE_APOSTR, TYPE_COMMA,
		TYPE_OCTOTHORPE, TYPE_TOKEN_LIST, TYPE_BASE_TOKEN, TYPE_NULL
	};

	int size = sizeof( remove ) / sizeof( type_t );
	ret = remove_token_list( tokens, remove, size );

	return ret;
}

// Removes all "metadata" tokens from a tree, which are only used in indentation-sensitive parsers
token_t *remove_meta_tokens( token_t *tokens ){
	token_t *ret = tokens;
	type_t remove[] = {
		TYPE_INDENT, TYPE_NEWLINE, TYPE_COLON, TYPE_PERIOD, TYPE_COMMA
	};

	int size = sizeof( remove ) / sizeof( type_t );
	ret = remove_token_list( tokens, remove, size );

	return ret;
}


bool has_shared_data( type_t type ){
	return type == TYPE_VARIABLE_REF || type == TYPE_PROCEDURE;
}

// Clones a single token
token_t *clone_token( const token_t *token ){
	token_t *ret = NULL;

	ret = alloc_token( );
	*ret = *token;

	//if ( has_shared_data( token->type )){
	if ( token->flags & T_FLAG_HAS_SHARED ){ 
		ret->data = shared_aquire( token->data );
	}
	
	return ret;
}

// Clones every token reachable from the given token
token_t *clone_tokens( const token_t *tree ){
	token_t *ret = NULL;

	if ( tree ){
		ret = clone_token( tree );

		ret->down = clone_tokens( tree->down );
		ret->next = clone_tokens( tree->next );
	}
	
	return ret;
}

// Clones a token and all lower nodes
token_t *clone_token_tree( const token_t *tree ){
	token_t *ret = NULL;

	if ( tree ){
		ret = clone_token( tree );

		ret->down = clone_tokens( tree->down );
		ret->next = NULL;
	}

	return ret;
}

// Clone only the topmost nodes of the tree, while keeping the lower nodes
token_t *clone_token_spine( const token_t *tree ){
	token_t *ret = NULL;

	if ( tree ){
		ret = clone_token( tree );

		ret->down = tree->down;
		ret->next = clone_tokens( tree->next );
	}

	return ret;
}

// Returns the "horizontal" length of a tree, or how many "next" tokens there are
unsigned tokens_length( const token_t *tree ){
	unsigned ret;
	const token_t *move = tree;

	for ( ret = 0; move; move = move->next, ret++ );

	return ret;
}

// Recursively replaces all symbol-type tokens named "name" with the "replace" token
token_t *replace_symbol( token_t *tokens, const token_t *replace, const char *name ){
	token_t *ret = tokens;

	if ( tokens ){
		if ( tokens->type == TYPE_SYMBOL && ( strcmp( shared_get( tokens->data ), name )) == 0 ){
			ret = clone_token_tree( replace );
			ret->next = replace_symbol( tokens->next, replace, name );
			free_token_tree( tokens );

		} else {
			ret->down = replace_symbol( ret->down, replace, name );
			ret->next = replace_symbol( ret->next, replace, name );
		}
	}
	
	return ret;
}

// Replaces tokens while preserving variable definitions in lambdas/procedures.
// TODO: remove this in the future, if no use is found for it
token_t *replace_symbol_safe( token_t *tokens, const token_t *replace, const char *name ){
	token_t *ret = tokens;

	if ( tokens ){
		if ( tokens->type == TYPE_SYMBOL && ( strcmp( tokens->data, name )) == 0 ){
			ret = clone_token_tree( replace );
			ret->next = replace_symbol_safe( tokens->next, replace, name );
			free_token_tree( tokens );

		} else if ( tokens->type == TYPE_LAMBDA ){
			ret->next->next = replace_symbol_safe( ret->next->next, replace, name );

		} else if ( tokens->type == TYPE_PROCEDURE ){
			ret->next = replace_symbol_safe( ret->next, replace, name );

		} else {
			ret->down = replace_symbol_safe( ret->down, replace, name );
			ret->next = replace_symbol_safe( ret->next, replace, name );
		}
	}
	
	return ret;
}

// Recursively replaces all tokens of "type" with "replace"
token_t *replace_type( token_t *tokens, const token_t *replace, type_t type ){
	token_t *ret = tokens;
	token_t *move;

	if ( tokens ){
		if ( tokens->type == type ){
			ret = clone_tokens( replace );
			for ( move = ret; move->next; move = move->next );
			move->next = replace_type( tokens->next, replace, type );
			free_token_tree( tokens );

		} else {
			ret->down = replace_type( ret->down, replace, type );
			ret->next = replace_type( ret->next, replace, type );
		}
	}
	
	return ret;
}
