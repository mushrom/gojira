#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gojira/lexer.h>

#define ALPHABET	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define DIGITS		"0123456789"
#define ALPHANUM	ALPHABET DIGITS
#define DELIMITER	"()[]{} "
#define IDENTIFIER	ALPHANUM "!@#$%^&*_-=+/?<>.~:"
#define SEPERATOR	" \t\n\r"

/* Used to return multiple values from get_token_from_str */
typedef struct token_return {
	token_t *token;
	char *string;
	bool found;
} token_return_t;

typedef bool (*char_pred)( char );

static token_return_t get_token_from_str( char *string );

// Extracts all possible tokens from the given string, returning a token tree
// with all "next" fields pointing to the next token (so no "down" fields will be set)
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

		foo = get_token_from_str( foo.string );
	}

	move->next = calloc( 1, sizeof( token_t ));
	move->next->type = TYPE_NULL;

	return temp.next;
}

// Checks whether the given character is part of an identifier
static inline bool is_identifier_char( char c ){
	bool ret = false;

	ret = (c & 0x80) ||                     // Check for utf8 chars
		( strchr( IDENTIFIER, c )) != NULL; // otherwise check for an ascii identifier char

	return ret;
}

// like strspn, except instead of testing for chars in "accept", it tests for chars
// for which predicate( char ) returns true.
static inline unsigned spanchars( char_pred predicate, char *s ){
	unsigned ret;

	for ( ret = 0; s[ret] && predicate( s[ret] ); ret++ );

	return ret;
}

/* returns a token_return_t struct giving information about the lexer's state.
 * If a token was found, the "token" field will point to a valid token, "found" will be true,
 *    and "string" will point to the next position after the returned token.
 * If a token wasn't found, "token" will be undefined, "found" will be false, and "string" undefined.
 *
 * TODO: possibly find a cleaner and less cumbersome way to return tokens.
 */
static token_return_t get_token_from_str( char *string ){
	token_return_t ret;
	char *temp, *foo;
	unsigned i;
	
	ret = (token_return_t){
		.token 	= calloc( 1, sizeof( token_t )),
		.string	= NULL,
		.found	= false,
	};

	for ( ; *string && strchr( SEPERATOR, *string ); string++ );

	if ( *string ){
		// Check for parenthesis
		if ( *string == '(' || *string == '{' || *string == '[' ){
			ret.string = string + 1;
			ret.token->type = TYPE_OPEN_PAREN;
			ret.found = true;

		} else if ( *string == ')' || *string == '}' || *string == ']' ){
			ret.string = string + 1;
			ret.token->type = TYPE_CLOSE_PAREN;
			ret.found = true;

		// Check for quoted things
		} else if ( *string == '\'' ){
			ret.string = string + 1;
			ret.token->type = TYPE_APOSTR;
			ret.found = true;

		// Check for commas, for special syntax sugar purposes
		} else if ( *string == ',' ){
			ret.string = string + 1;
			ret.token->type = TYPE_COMMA;
			ret.found = true;

		// Check for periods and special conditions ("." signifies a pair, while ".." is an identifier)
		} else if ( *string == '.' && strchr( SEPERATOR, string[1])){
			ret.string = string + 1;
			ret.token->type = TYPE_PERIOD;
			ret.found = true;

		// Check for strings
		} else if ( *string == '"' ){
			//ret.string = strchr( string + 1, '"' );
			ret.string = temp = string + 1;
			for ( i = 0; temp[i] && temp[i] != '"'; i++ );

			foo = malloc( sizeof( char[i + 1] ));

			strncpy( foo, temp, i );
			foo[i] = 0;

			ret.token->data = foo;

			if ( ret.string ){
				ret.string += i + 1;
				ret.token->type = TYPE_STRING;
				ret.found = true;

			} else {
				//TODO: let the user know they done messed up
			}

		} else if ( *string == '#' ){
			/* could be either vector, boolean or character
			 *
			 * vectors are parsed after lexing, so just leave
			 * the octothorpe if it's not a boolean or character.
			 */

			// Check for booleans
			if ( string[1] == 'f' || string[1] == 't' ){
				ret.string = string + 2;
				ret.token->type = TYPE_BOOLEAN;
				ret.token->smalldata = (string[1] == 't')? true : false;
				ret.found = true;

			// Check for characters
			} else if ( string[1] == '\\' && string[2] ){
				ret.string = string + 3;
				ret.token->type = TYPE_CHAR;
				ret.token->smalldata = string[2];
				ret.found = true;

			// Otherwise just leave everything
			} else {
				ret.string = string + 1;
				ret.token->type = TYPE_OCTOTHORPE;
				ret.found = true;
			}

		// Check for comments
		} else if ( *string == ';' ){
			for ( i = 0; string[i] && string[i + 1] != '\n'; i++ );
			ret = get_token_from_str( string + i + 1 );

		// Check for numbers
		} else if ( strchr( DIGITS, *string ) ||
				( *string == '-' && strchr( DIGITS, *(string + 1)))) {

			i = strspn( string, "-"DIGITS );
			temp = malloc( sizeof( char[i + 1]));
			strncpy( temp, string, i );

			ret.string = string + i;
			ret.token->type = TYPE_NUMBER;
			ret.found = true;
			ret.token->smalldata = atoi( temp );

			free( temp );

		// Check for an identifier
		} else if ( is_identifier_char( *string )){
			i = spanchars( is_identifier_char, string );

			ret.string = string + i;
			ret.token->type = TYPE_SYMBOL;
			ret.found = true;

			temp = ret.token->data = malloc( sizeof( char[i + 1] ));
			strncpy( ret.token->data, string, i );
			temp[i] = 0;

			//printf( "> Have identifier \"%s\"\n", temp );

			if ( strcmp( temp, "lambda" ) == 0   || strcmp( temp, "λ" ) == 0 ||
			     strcmp( temp, "function" ) == 0 || strcmp( temp, "func" ) == 0 ){
				ret.token->type = TYPE_LAMBDA;

			} else if ( strcmp( temp, "if" ) == 0 ){
				ret.token->type = TYPE_IF;

			} else if ( strcmp( temp, "define-syntax" ) == 0 ){
				ret.token->type = TYPE_DEF_SYNTAX;

			} else if ( strcmp( temp, "syntax-rules" ) == 0 ){
				ret.token->type = TYPE_SYNTAX_RULES;
			}
		}
	}

	// If no token is found, clean up
	if ( !ret.found )
		free( ret.token );

	return ret;
} 