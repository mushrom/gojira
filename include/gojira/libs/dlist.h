#ifndef _GOJIRA_DLIST_H
#define _GOJIRA_DLIST_H 1

#ifdef __cplusplus
extern "C" {
#endif

#define DLIST_DEFAULT_BLOCKSIZE 8
#define foreach_in_dlist( iter, list ) \
	for( iter = 0; i < list->used; i++ )

enum {
	DLIST_FLAG_NULL
};

typedef struct dlist {
	void 		**entries;
	unsigned 	used;
	unsigned 	alloced;
	unsigned 	block_size;
	unsigned 	blocks_alloced;

	unsigned 	max_alloc;
	unsigned 	flags;
} dlist_t;

dlist_t *dlist_create( unsigned block_size, unsigned prealloc );
void dlist_free( dlist_t *list );

int dlist_add( dlist_t *list, void *data );
int dlist_remove( dlist_t *list, int index );
void *dlist_get( dlist_t *list, int index );
void *dlist_set( dlist_t *list, int index, void *data );

unsigned dlist_allocated( dlist_t *list );
unsigned dlist_used( dlist_t *list );

#ifdef __cplusplus
}
#endif
#endif
