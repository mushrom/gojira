#ifndef _parse_h
#define _parse_h
#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gojira/tokens.h>

typedef struct rule {
	type_t	type;
	type_t	ret;

	struct rule	*next;
	struct rule	*down;
} rule_t;

token_t *parse_tokens( token_t * );

/*
token_t *baseline( token_t * );
token_t *maptoken( token_t *, type_t );
token_t *reduce( token_t * );
token_t *reduceto( token_t *, type_t );
*/
token_t *reduce( token_t *, type_t );
type_t predict( token_t * );
rule_t *gen_cminus_rules( );
token_t *parse_scheme_tokens( char *buf );
token_t *parse_mlisp_tokens( char *buf );
token_t *preprocess_mlisp( token_t *tokens );

#ifdef __cplusplus
}
#endif
#endif
