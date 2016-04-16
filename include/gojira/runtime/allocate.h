#ifndef _GOJIRA_RUNTIME_ALLOCATE_H
#define _GOJIRA_RUNTIME_ALLOCATE_H
#include <gojira/tokens.h>
#include <gojira/runtime/garbage.h>

token_t *alloc_token( void );
token_t *alloc_token_nozero( void );
void *alloc_block( void );
void *alloc_block_nozero( void );
void cache_blocks( gbg_list_t *list );

void free_node( gbg_node_t *node );
void free_token( token_t *token );
void free_tokens( token_t *tree );
void free_token_tree( token_t *tree );

void destroy_token_cache( );

#endif
