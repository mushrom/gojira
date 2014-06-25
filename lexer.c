#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <gojira/tokens.h>

/* Used to return multiple values from get_token_from_str */
struct token_return {
	token_t *token;
	char *string;
};

static struct token_return get_token_from_str( char *string ){
	struct token_return ret = {
		.token 	= calloc( 1, sizeof( token_t )),
		.string	= NULL,
	};

	for ( ; *string == ' ' || *string == '\n' ||
		*string == '\t' || *string == '\r'; string++ );

	if ( *string ){
		if ( *string == '(' ){
			ret.string = string + 1;
			ret.token->type = TYPE_OPEN_PAREN;

		} else if ( *string == ')' ){
			ret.string = string + 1;
			ret.token->type = TYPE_CLOSE_PAREN;

		} else if ( *string == '"' ){
			ret.string = strchr( string + 1, '"' );
			ret.token->type = TYPE_STRING;

		} else if ( *string == '#' ){
			// could be either boolean or character
			if ( string[1] == 'f' || string[1] == 't' ){
				ret.string = string + 2;
				ret.token->type = TYPE_BOOLEAN;

			} else if ( string[1] == '\\' ){
				ret.string = string + 3;
				ret.token->type = TYPE_CHAR;
			}

		} else if ( *string > ' ' && *string < 0x7f ){
			ret.string = string + 1;
			ret.token->type = TYPE_SYMBOL;
		}
	}

	if ( !ret.string )
		free( ret.token );

	return ret;
} 

token_t *lexerize( char *string ){
	token_t *ret = NULL;
	token_t *move;
	token_t temp;

	move = &temp;

	return ret;
}

