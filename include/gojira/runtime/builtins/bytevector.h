#ifndef _GOJIRA_BYTEVECTOR_H
#define _GOJIRA_BYTEVECTOR_H 1
#include <stdlib.h>
#include <stdint.h>

typedef struct bytevector {
	size_t length;
	union {
		uint8_t *bytes;
		void *data;
	};

} bytevector_t;

token_t *builtin_make_bytevector( stack_frame_t *frame );
token_t *builtin_bytevector_u8_ref( stack_frame_t *frame );
token_t *builtin_bytevector_length( stack_frame_t *frame );
token_t *builtin_bytevector_from_u8s( stack_frame_t *frame );

#endif
