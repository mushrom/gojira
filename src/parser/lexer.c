#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gojira/lexer.h>
#include <gojira/libs/shared.h>

#define ALPHABET	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define DIGITS		"0123456789"
#define ALPHANUM	ALPHABET DIGITS
#define DELIMITER	"()[]{} "
#define IDENTIFIER	ALPHANUM "!@#$%^&*_-=+/?<>.~:"
#define SEPERATOR	" \t\n\r"
#define INDENT		" \t"
#define NEWLINE     "\n\r"

/* Used to return multiple values from get_token_from_str */
typedef struct token_return {
	token_t *token;
	const char *string;
	bool found;
} token_return_t;

typedef bool (*char_pred)( char );

static token_return_t get_token_from_str( const char *string );

void free_string( void *ptr ){
	//printf( "[%s] Freeing string \"%s\"\n", __func__, (char *)ptr );
	free( ptr );
}

// Extracts all possible tokens from the given string, returning a token tree
// with all "next" fields pointing to the next token (so no "down" fields will be set)
token_t *lexerize( const char *string ){
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
static inline unsigned spanchars( char_pred predicate, const char *s ){
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
 *       split into smaller functions.
 */
static token_return_t get_token_from_str( const char *string ){
	token_return_t ret;
	const char *temp;
	char *foo;
	unsigned i, indent = 0;
	
	ret = (token_return_t){
		.token 	= calloc( 1, sizeof( token_t )),
		.string	= NULL,
		.found	= false,
	};

	for ( indent = 0; *string && strchr( INDENT, *string ); string++, indent++ );
	//for ( ; *string && strchr( SEPERATOR, *string ); string++ );

	if ( indent ){
		ret.string = string;
		ret.token->type = TYPE_INDENT;
		ret.token->smalldata = indent;
		ret.found = true;

	} else if ( *string ){
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

		} else if ( strchr( NEWLINE, *string )){
			ret.string = string + 1;
			ret.token->type = TYPE_NEWLINE;
			ret.found = true;

		} else if ( *string == ':' && strchr( SEPERATOR, string[1] )){
			ret.string = string + 1;
			ret.token->type = TYPE_COLON;
			ret.found = true;

		// Check for strings
		} else if ( *string == '"' ){
			bool escaped = false;
			ret.string = temp = string + 1;

			// Check to make sure the string is proper
			for ( i = 0; temp[i] && (!escaped && temp[i] == '"') == false; i++ ){
				if ( temp[i] == '\\' && !escaped ){
					escaped = true;
				} else {
					escaped = false;
				}
			}

			if ( temp[i] ){
				unsigned n;
				unsigned real_pos;
				foo = malloc( sizeof( char[i + 1] ));

				// Copy the string to it's own buffer, while escaping characters
				escaped = false;
				for ( real_pos = n = 0; temp[n] && n < i; n++ ){
					if ( temp[n] == '\\' && !escaped ){
						escaped = true;
					} else {
						foo[real_pos] = temp[n];
						escaped = false;
						real_pos++;
					}
				}

				foo[real_pos] = 0;

				//ret.token->data = foo;
				ret.token->data = shared_new( foo, free_string );
				ret.token->flags = T_FLAG_HAS_SHARED;
				ret.string += i + 1;
				ret.token->type = TYPE_STRING;
				ret.found = true;

			} else {
				//TODO: keep track of where the current token is
				printf( "[%s] Error: Unterminated string at $place\n", __func__ );
			}

		// Check for comments
		} else if ( *string == ';' || (*string == '#' && string[1] == '!')){
			for ( i = 0; string[i] && string[i + 1] != '\n'; i++ );
			ret = get_token_from_str( string + i + 1 );

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
				if ( strncmp( string + 2, "newline", 7 ) == 0 ){
					ret.string = string + 9;
					ret.token->type = TYPE_CHAR;
					ret.token->smalldata = '\n';
					ret.found = true;

				} else if ( strncmp( string + 2, "return", 6 ) == 0 ){
					ret.string = string + 8;
					ret.token->type = TYPE_CHAR;
					ret.token->smalldata = '\r';
					ret.found = true;

				} else if ( strncmp( string + 2, "space", 5 ) == 0 ){
					ret.string = string + 7;
					ret.token->type = TYPE_CHAR;
					ret.token->smalldata = ' ';
					ret.found = true;

				} else if ( strncmp( string + 2, "tab", 3 ) == 0 ){
					ret.string = string + 5;
					ret.token->type = TYPE_CHAR;
					ret.token->smalldata = '\t';
					ret.found = true;

				} else if ( strncmp( string + 2, "escape", 6 ) == 0 ){
					ret.string = string + 8;
					ret.token->type = TYPE_CHAR;
					ret.token->smalldata = '\x1b';
					ret.found = true;

				} else if ( strncmp( string + 2, "backspace", 9 ) == 0 ){
					ret.string = string + 11;
					ret.token->type = TYPE_CHAR;
					ret.token->smalldata = '\b';
					ret.found = true;

				} else {
					ret.string = string + 3;
					ret.token->type = TYPE_CHAR;
					ret.token->smalldata = string[2];
					ret.found = true;
				}

			// Otherwise just leave everything
			} else {
				ret.string = string + 1;
				ret.token->type = TYPE_OCTOTHORPE;
				ret.found = true;
			}

		// Check for numbers
		} else if ( strchr( DIGITS, *string ) ||
				( *string == '-' && strchr( DIGITS, *(string + 1)))) {

			i = strspn( string, "-"DIGITS );
			foo = malloc( sizeof( char[i + 1]));
			strncpy( foo, string, i );

			ret.string = string + i;
			ret.token->type = TYPE_NUMBER;
			ret.found = true;
			ret.token->smalldata = atoi( foo );

			free( foo );

		// Check for an identifier
		} else if ( is_identifier_char( *string )){
			i = spanchars( is_identifier_char, string );

			ret.string = string + i;
			ret.found = true;

			foo = malloc( sizeof( char[i + 1] ));
			strncpy( foo, string, i );
			foo[i] = 0;

			if ( strcmp( foo, "lambda" ) == 0   || strcmp( foo, "λ" ) == 0 ||
			     strcmp( foo, "function" ) == 0 || strcmp( foo, "func" ) == 0 ){
				ret.token->type = TYPE_LAMBDA;
				free( foo );

			} else if ( strcmp( foo, "syntax-rules" ) == 0 ){
				ret.token->type = TYPE_SYNTAX_RULES;
				free( foo );

			} else {
				ret.token->type = TYPE_SYMBOL;
				ret.token->data = shared_new( foo, free_string );
				ret.token->flags = T_FLAG_HAS_SHARED;
			}
		}
	}

	// If no token is found, clean up
	if ( !ret.found )
		free( ret.token );

	return ret;
} 
