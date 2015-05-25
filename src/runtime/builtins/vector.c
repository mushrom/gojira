#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>
#include <gojira/libs/dlist.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

token_t *builtin_is_vector( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens - 1 == 1 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;
		ret->smalldata = frame->expr->next->type == TYPE_VECTOR;

	} else {
		frame->error_call( frame,
				"[%s] Error: Expected 1 argument, but have %d\n",
				__func__,
				frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_vector_ref( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *foo;
	shared_t *shr;
	dlist_t *dlst;
	
	if ( frame->ntokens - 1 == 2 ){
		if ( frame->expr->next->type == TYPE_VECTOR &&
		     frame->expr->next->next->type == TYPE_NUMBER ){

			foo = frame->expr->next->next;
			shr = frame->expr->next->data;
			dlst = shared_get( shr );

			if ( foo->smalldata < dlist_used( dlst )){
				ret = clone_tokens( dlist_get( dlst, foo->smalldata ));

			} else {
				frame->error_call( frame, "[%s] Error: Index is out of range\n", __func__ );
			}

		} else {
			frame->error_call( frame,
					"[%s] Error: Expected vector and int as arguments, but have \"%s\" and \"%s\"\n",
					__func__,
					type_str( frame->expr->next->type ),
					type_str( frame->expr->next->next->type ));
		}

	} else {
		frame->error_call( frame,
			"[%s] Error: expected 2 arguments, but have %d\n",
			__func__,
			frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_vector_set( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *foo;
	token_t *elem;
	shared_t *shr;
	dlist_t *dlst;
	
	if ( frame->ntokens - 1 == 3 ){
		if ( frame->expr->next->type == TYPE_VECTOR &&
		     frame->expr->next->next->type == TYPE_NUMBER ){

			foo = frame->expr->next->next;
			shr = frame->expr->next->data;
			dlst = shared_get( shr );

			if ( foo->smalldata < dlist_used( dlst )){
				elem = clone_token_tree( frame->expr->next->next->next );
				dlist_set( dlst, foo->smalldata, elem );

				ret = frame->expr->next;

			} else {
				frame->error_call( frame, "[%s] Error: Index is out of range\n", __func__ );
			}

		} else {
			frame->error_call( frame,
				"[%s] Error: Expected vector and int as arguments, but have \"%s\" and \"%s\"\n",
				__func__,
				type_str( frame->expr->next->type ),
				type_str( frame->expr->next->next->type ));
		}

	} else {
		frame->error_call( frame, "[%s] asdf\n", __func__ );
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
			ret->smalldata = dlist_used( dlst );

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
