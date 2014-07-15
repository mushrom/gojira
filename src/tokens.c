#include <stdio.h>
#include <stdlib.h>
#include <gojira/parse_debug.h>
#include <gojira/tokens.h>

token_t *dump_tokens( token_t *tokens, int level ){
	if ( tokens ){
		int i;
		for ( i = 0; i < level * 4; i++ )
			putchar( ' ' );

		printf( "%s", type_str( tokens->type ));
		switch( tokens->type ){
			case TYPE_SYMBOL:
				printf( ": %s", tokens->data );
				break;
			case TYPE_CHAR:
				printf( ": %c", tokens->smalldata );
				break;
			default:
				break;
		}
		printf( "\n" );
		//printf( "Token, type: %d\n", tokens->type );
		dump_tokens( tokens->down, level + 1 );
		dump_tokens( tokens->next, level );
	}

	return tokens;
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

token_t *remove_punc_tokens( token_t *tokens ){
	token_t *ret = tokens;

	if ( ret ){
		ret = strip_token( ret, TYPE_OPEN_PAREN );
		ret = strip_token( ret, TYPE_CLOSE_PAREN );
		ret = strip_token( ret, TYPE_APOSTR );
		ret = strip_token( ret, TYPE_OCTOTHORPE );
		ret = strip_token( ret, TYPE_TOKEN_LIST );
		ret = strip_token( ret, TYPE_BASE_TOKEN );
		ret = strip_token( ret, TYPE_NULL );

		ret->down = remove_punc_tokens( ret->down );
		ret->next = remove_punc_tokens( ret->next );
	}

	return ret;
}

// Clones every token reachable from the given token
token_t *clone_tokens( token_t *tree ){
	token_t *ret = NULL;

	if ( tree ){
		ret = calloc( 1, sizeof( token_t ));
		// TODO: copy tree->data or tree->smalldata depending on the type
		memcpy( ret, tree, sizeof( token_t ));

		ret->down = clone_tokens( tree->down );
		ret->next = clone_tokens( tree->next );
	}
	
	return ret;
}

// Clones a token and all lower nodes
token_t *clone_token_tree( token_t *tree ){
	token_t *ret = NULL;

	if ( tree ){
		ret = calloc( 1, sizeof( token_t ));
		memcpy( ret, tree, sizeof( token_t ));

		ret->down = clone_tokens( tree->down );
		ret->next = NULL;
	}

	return ret;
}

unsigned tokens_length( token_t *tree ){
	unsigned ret;
	token_t *move = tree;

	for ( ret = 0; move; move = move->next, ret++ );

	return ret;
}
