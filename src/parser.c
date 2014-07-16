#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gojira/parser.h>
#include <gojira/parse_debug.h>
#include <gojira/scheme_rules.h>

// Debugging function, to make sure the rule tables are being generated properly
void dump_rules( int level, rule_t *rules ){
	if ( rules ){
		int i;
		for ( i = 0; i < level; i++ )
			printf( "    " );
		
		printf( "%s -> %s\n", type_str( rules->type ), type_str( rules->ret ));
		dump_rules( level+1, rules->down );
		dump_rules( level, rules->next );
	}
}

// Returns the last node in a reduction, with the status set to the returning type
token_t *baseline_iter( token_t *tokens, rule_t *rules ){
	token_t	*ret,
			*move;
	type_t	type;
	rule_t		*rmove;

	ret = move = tokens;
	rmove = rules;
	int found = 0;

	if ( move ){
		for ( ; !found && rmove; rmove = rmove->next ){
			if ( rmove->type == move->type ){
				type = rmove->ret;

				if ( rmove->down ){
					if ( move->next->type != rmove->down->type )
						move->next = reduce( move->next, rmove->down->type );

					ret = baseline_iter( move->next, rmove->down );
					if ( ret->status == TYPE_NULL && type != TYPE_NULL ){
						ret = move;
						ret->status = type;
					}
				} else {
					move->status = type;
					ret = move;
				}

				found = 1;
				break;
			}
		}
	}

	return ret;
}

// Performs one round of reduction
token_t *baseline( token_t *tokens, rule_t *rules ){
	token_t	*ret,
			*move,
			*temp;
	rule_t		*rmove;

	ret = move = tokens;
	rmove = rules;

	move = baseline_iter( move, rules );

	if ( move->status != TYPE_NULL ){
		temp = calloc( 1, sizeof( token_t ));
		temp->type = move->status;
		temp->down = ret;

		temp->next = move->next;
		move->next = NULL;
		move->status = TYPE_NULL;

		ret = temp;
	}

	return ret;
}

int has_higher_prec( type_t top, type_t bottom, rule_t *rules ){
	rule_t *ruleptr = rules;
	int ret, found;

	for ( ret = found = 0; !found && ruleptr->next; ruleptr = ruleptr->next ){
		if ( ruleptr->type == top ){
			ret = 1;
			found = 1;
		} else if ( ruleptr->type == bottom ){
			ret = 0;
			found = 1;
		}
	}

	return ret;
}

// Repeatedly reduces until the returning token is either the topmost expression possible, 
// or is of type "type"
token_t *reduce( token_t *tokens, type_t type ){
	token_t	*ret = tokens,
			*move = tokens;

	while (( ret = baseline( move, scheme_rules )) && ret != move && ret->type != type )
		move = ret;

	return ret;
}

token_t *parse_tokens( token_t *tokens ){
	token_t *ret = NULL;

	//printf( "-=[ Rules dump: \n" );
	//dump_rules( 0, scheme_rules );
	ret = reduce( tokens, TYPE_NULL );

	return ret;
}
