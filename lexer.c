#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gojira/lexer.h>

/* Used to return multiple values from get_token_from_str */
typedef struct token_return {
	token_t *token;
	char *string;
	bool found;
} token_return_t;

static token_return_t get_token_from_str( char *string );

token_t *lexerize( char *string ){
	token_t *move;
	token_t temp;
	token_return_t foo;

	temp.next = NULL;
	move = &temp;

	foo = get_token_from_str( string );
	while ( foo.found ){
		move->next = foo.token;
		move = move->next;
		printf( ": %s", foo.string );

		foo = get_token_from_str( foo.string );
	}

	return temp.next;
}

static token_return_t get_token_from_str( char *string ){
	unsigned i;
	token_return_t ret;
	
	ret = (token_return_t){
		.token 	= calloc( 1, sizeof( token_t )),
		.string	= NULL,
		.found	= false,
	};

	for ( ; *string == ' ' || *string == '\n' ||
		*string == '\t' || *string == '\r'; string++ );

	if ( *string ){
		if ( *string == '(' ){
			ret.string = string + 1;
			ret.token->type = TYPE_OPEN_PAREN;
			ret.found = true;

		} else if ( *string == ')' ){
			ret.string = string + 1;
			ret.token->type = TYPE_CLOSE_PAREN;
			ret.found = true;

		} else if ( *string == '\'' ){
			ret.string = string + 1;
			ret.token->type = TYPE_APOSTR;
			ret.found = true;

		} else if ( *string == '"' ){
			ret.string = strchr( string + 1, '"' );

			if ( ret.string ){
				ret.string++;
				ret.token->type = TYPE_STRING;
				ret.found = true;
			} else {
				//TODO: let the user know they done messed up
			}

		} else if ( *string == '#' ){
			// could be either vector, boolean or character
			//
			// vectors are parsed after lexing, so just leave
			// the octothorpe if it's not a boolean or character.
			if ( string[1] == 'f' || string[1] == 't' ){
				ret.string = string + 2;
				ret.token->type = TYPE_BOOLEAN;
				ret.token->smalldata = (string[1] == 't')? true : false;
				ret.found = true;

			} else if ( string[1] == '\\' && string[2] ){
				ret.string = string + 3;
				ret.token->type = TYPE_CHAR;
				ret.token->smalldata = string[2];
				ret.found = true;

			} else {
				ret.string = string + 1;
				ret.token->type = TYPE_OCTOTHORPE;
				ret.found = true;
			}

		} else if ( *string > 'A' && *string < 'z'
			/* TODO: proper delimiting here */ ){

			for ( i = 0; string[i] && string[i] >= 'A' && string[i] <= 'z'; i++ );
			ret.string = string + i;
			ret.token->type = TYPE_SYMBOL;
			ret.found = true;
		}
	}

	if ( !ret.found )
		free( ret.token );

	return ret;
} 
