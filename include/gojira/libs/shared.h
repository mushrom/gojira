#ifndef _GOJIRA_SHARED_H
#define _GOJIRA_SHARED_H
#include <stdlib.h>

#define NO_DTOR      NULL
#define DEFAULT_DTOR free

typedef void (*shared_dtor)( void *ptr );

typedef struct shared {
	void *data;
	unsigned references;
	shared_dtor dtor;
	// semaphore here, maybe
} shared_t;

shared_t *shared_new( void *data, shared_dtor dtor );

void *shared_get( shared_t *ptr );
shared_t *shared_aquire( shared_t *ptr );
void  shared_release( shared_t *ptr );

#endif
