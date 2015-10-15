#include <gojira/runtime/builtin.h>
#include <gojira/libs/numbers.h>
#include <gojira/parse_debug.h>
#include <gojira/runtime/builtins/bytevector.h>

#include <stdint.h>
#include <string.h>

void free_bytevector( void *buf ){
	bytevector_t *bytevec = buf;

	printf( "[%s] got here\n", __func__ );

	free( bytevec->bytes );
	free( bytevec );
}

static bytevector_t *make_bytevector( size_t size ){
	size_t alloc_size;
	bytevector_t *ret;

	// make sure allocation size is a multiple of 8, for conversion reasons
	alloc_size = (size < 8)? 8 : size + ((size % 8)? 8 - size % 8 : 0);

	ret = malloc( sizeof( bytevector_t ));
	ret->bytes = calloc( 1, alloc_size );
	ret->length = size;

	//printf( "[%s] allocated %lu bytes for size %lu\n", __func__, alloc_size, size );

	return ret;
}

token_t *builtin_make_bytevector( stack_frame_t *frame ){
	token_t *ret = NULL;
	bytevector_t *bytevec = NULL;
	shared_t *shr = NULL;
	uint8_t setval = 0;
	size_t len = 0;
	bool error = false;

	if ( frame->ntokens >= 2 ){
		token_t *move = frame->expr->next;

		if ( move->type == TYPE_NUMBER ){
			len = move->number.s_int;

			if ( frame->ntokens == 3 ){
				if ( move->next->type == TYPE_NUMBER ){
					setval = (uint8_t)move->next->number.u_int;

				} else {
					FRAME_ERROR_ARGTYPE( frame, "integer", move->next->type );
					error = true;
				}
			} 

			if ( !error ){
				/*
				bytevec = malloc( sizeof( bytevector_t ));
				bytevec->bytes = malloc( len );
				bytevec->length = len;
				*/
				bytevec = make_bytevector( len );
				if ( setval ){
					memset( bytevec->bytes, setval, len );
				}

				shr = shared_new( bytevec, free_bytevector );

				ret = alloc_token( );
				ret->type = TYPE_BYTEVECTOR;
				ret->data = shr;
				ret->flags |= T_FLAG_HAS_SHARED;
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "integer", move->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

token_t *builtin_bytevector_u8_ref( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		token_t *temp  = frame->expr->next;
		token_t *index = frame->expr->next->next;

		if ( temp->type == TYPE_BYTEVECTOR ){
			if ( index->type == TYPE_NUMBER ){
				bytevector_t *bytevec = shared_get( temp->data );

				if ( index->number.u_int < bytevec->length ){
					ret = alloc_token( );
					ret->type = TYPE_NUMBER;
					ret->number = as_int_number( bytevec->bytes[ index->number.u_int ]);

				} else {
					FRAME_ERROR( frame, "index %lu out of range", index->number.u_int );
				}

			} else {
				FRAME_ERROR_ARGTYPE( frame, "number", index->type );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "bytevector", temp->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_bytevector_length( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *temp = frame->expr->next;

		if ( temp->type == TYPE_BYTEVECTOR ){
			bytevector_t *bytevec = shared_get( temp->data );

			ret = alloc_token( );
			ret->type = TYPE_NUMBER;
			ret->number = as_int_number( bytevec->length );

		} else {
			FRAME_ERROR_ARGTYPE( frame, "bytevector", temp->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

token_t *builtin_bytevector_from_u8s( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *temp  = frame->expr->next;

		if ( temp->type == TYPE_LIST ){
			unsigned len;
			shared_t *shr;
			bytevector_t *bytevec;
			unsigned i;
			bool have_error = false;

			temp = temp->down;
			len = tokens_length( temp );
			bytevec = make_bytevector( len );
			shr = shared_new( bytevec, free_bytevector );

			for ( i = 0; temp; i++, temp = temp->next ){
				if ( temp->type == TYPE_NUMBER ){
					bytevec->bytes[i] = (uint8_t)temp->number.u_int;

				} else {
					FRAME_ERROR_ARGTYPE( frame, "integer", temp->type );
					have_error = true;
					break;
				}
			}

			if ( !have_error ){
				ret = alloc_token( );
				ret->type = TYPE_BYTEVECTOR;
				ret->data = shr;
				ret->flags |= T_FLAG_HAS_SHARED;
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "list", temp->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}
