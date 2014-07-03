#include <stdio.h>
#include <stdlib.h>
#include <gojira/parse_debug.h>
#include <gojira/tokens.h>

token_t *dump_tokens( token_t *tokens, int level ){
	if ( tokens ){
		int i;
		for ( i = 0; i < level * 4; i++ )
			putchar( ' ' );

		printf( "%s\n", type_str( tokens->type ));
		//printf( "Token, type: %d\n", tokens->type );
		dump_tokens( tokens->down, level + 1 );
		dump_tokens( tokens->next, level );
	}

	return tokens;
}

token_t *strip_token( token_t *tokens, type_t type ){
	token_t *ret = tokens;

	if ( tokens ){
		if ( tokens->type == type ){
			ret = strip_token( tokens->next, type );
			free( tokens );
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

		ret->down = remove_punc_tokens( ret->down );
		ret->next = remove_punc_tokens( ret->next );
	}

	return ret;
}
