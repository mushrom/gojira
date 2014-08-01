#ifndef _GOJIRA_RUNTIME_ALLOCATE_H
#define _GOJIRA_RUNTIME_ALLOCATE_H
#include <gojira/tokens.h>

token_t *alloc_token( void );
void free_tokens( token_t *tree );
void free_token_tree( token_t *tree );

void destroy_token_cache( );

#endif
