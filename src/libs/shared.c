#include <stdio.h>
#include <gojira/libs/shared.h>

shared_t *shared_new( void *data, shared_dtor dtor ){
	shared_t *ret = calloc( 1, sizeof( shared_t ));

	ret->data = data;
	ret->dtor = dtor;
	ret->references = 1;

	return ret;
}

void *shared_get( shared_t *ptr ){
	if ( ptr ){
		return ptr->data;
	}

	return NULL;
}

shared_t *shared_aquire( shared_t *ptr ){
	if ( ptr ){
		printf( "[%s] Shared variable with data %p now has %d references\n",
				__func__, ptr->data, ptr->references );
		ptr->references++;
	}

	return ptr;
}

void shared_release( shared_t *ptr ){
	if ( ptr && ptr->references ){
		ptr->references--;

		printf( "[%s] Shared variable with data %p now has %d references\n",
				__func__, ptr->data, ptr->references );

		if ( ptr->references == 0 ){
			if ( ptr->dtor ){
				ptr->dtor( ptr->data );
			}

			free( ptr );
		}

	} else {
		printf( "[%s] /!\\ Shared variable with data %p has too many releases!\n",
				__func__, ptr->data );
	}
}
