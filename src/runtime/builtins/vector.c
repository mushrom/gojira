#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/dlist.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// defined in src/runtime/syntax.c
void free_vector( void *ptr );

token_t *builtin_is_vector( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;
		ret->boolean = frame->expr->next->type == TYPE_VECTOR;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_vector_ref( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *foo;
	shared_t *shr;
	dlist_t *dlst;
	
	if ( frame->ntokens == 3 ){
		if ( frame->expr->next->type == TYPE_VECTOR ) {
			if ( frame->expr->next->next->type == TYPE_NUMBER ) {
				foo = frame->expr->next->next;
				shr = frame->expr->next->data;
				dlst = shared_get( shr );

				if ( foo->number.u_int < dlist_used( dlst )){
					ret = clone_tokens( dlist_get( dlst, foo->number.u_int ));

				} else {
					frame->error_call( frame, "[%s] Error: Index is out of range\n", __func__ );
				}

			} else {
				FRAME_ERROR_ARGTYPE( frame, "int", frame->expr->next->next->type );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "vector", frame->expr->next->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_vector_set( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *foo;
	token_t *elem;
	shared_t *shr;
	dlist_t *dlst;
	
	if ( frame->ntokens == 4 ){
		if ( frame->expr->next->type == TYPE_VECTOR ) {
			if ( frame->expr->next->next->type == TYPE_NUMBER ){
				foo = frame->expr->next->next;
				shr = frame->expr->next->data;
				dlst = shared_get( shr );

				if ( foo->number.u_int < dlist_used( dlst )){
					elem = clone_token_tree( frame->expr->next->next->next );
					dlist_set( dlst, foo->number.u_int, elem );

					ret = frame->expr->next;

				} else {
					frame->error_call( frame, "[%s] Error: Index is out of range\n", __func__ );
				}

			} else {
				FRAME_ERROR_ARGTYPE( frame, "number", frame->expr->next->next->type );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "vector", frame->expr->next->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 3 );
	}

	return ret;
}

token_t *builtin_vector_length( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	dlist_t *dlst;

	if ( frame->ntokens == 2 ){
		move = frame->expr->next;

		if ( move->type == TYPE_VECTOR ){
			dlst = shared_get( move->data );

			ret = alloc_token( );
			ret->type = TYPE_NUMBER;
			ret->number = as_int_number( dlist_used( dlst ));

		} else {
			frame->error_call( frame,
				"[%s] Error: Expected vector as argument, but have \"%s\"\n",
				__func__,
				type_str( move->type ));
		}

	} else {
		frame->error_call( frame,
			"[%s] Expected 1 argument, but have %d\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_make_vector( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		token_t *move = frame->expr->next;

		if ( move->type == TYPE_NUMBER ){
			if ( move->number.s_int >= 0 ){
				dlist_t *nlist = dlist_create( 1, move->number.u_int );
				shared_t *shr = shared_new( nlist, free_vector );
				unsigned i;

				for ( i = 0; i < move->number.u_int; i++ ){
					token_t *temp = clone_token_tree( move->next );
					dlist_add( nlist, temp );
				}

				ret = alloc_token( );
				ret->type = TYPE_VECTOR;
				ret->data = shr;
				ret->flags |= T_FLAG_HAS_SHARED;

			} else {
				FRAME_ERROR( frame, "cannot make negatively-sized vector %d", move->number.s_int );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "number", move->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_vector_from_list( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *move = frame->expr->next;

		if ( move->type == TYPE_LIST ){
			unsigned len;
			token_t *temp;
			dlist_t *nlist;
			shared_t *shr;

			move = move->down;
			len = tokens_length( move );
			nlist = dlist_create( 1, len );
			shr = shared_new( nlist, free_vector );

			for ( ; move; move = move->next ){
				temp = clone_token_tree( move );
				dlist_add( nlist, temp );
			}

			ret = alloc_token( );
			ret->type = TYPE_VECTOR;
			ret->data = shr;
			ret->flags |= T_FLAG_HAS_SHARED;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "list", move->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}
