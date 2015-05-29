#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void iterator_free( void *ptr ){
	iterator_t *iter = ptr;
	//printf( "[%s] Got here, %p\n", __func__, ptr );
	DEBUGP( "[%s] Got here, %p\n", __func__, ptr );
	shared_release( iter->procedure );
	free( ptr );
}

token_t *builtin_iterator( stack_frame_t *frame ){
	token_t *temp = frame->expr->next;
	token_t *ret = NULL;
	iterator_t *iter;

	if ( frame->ntokens == 2 ){
		if ( temp->type == TYPE_PROCEDURE ){
			iter = malloc( sizeof( iterator_t ));
			iter->procedure = shared_aquire( temp->data );
			iter->counter = 0;
			iter->limit = 0;

			ret = alloc_token( );
			ret->type = TYPE_ITERATOR;
			ret->flags = T_FLAG_HAS_SHARED;
			ret->data = shared_new( iter, iterator_free );

			DEBUGP( "[%s] Returning new iterator at %p (%p)\n", __func__, ret->data, iter );
		}

	} else if ( frame->ntokens == 3 ) {
		if ( temp->type == TYPE_PROCEDURE ){
			if ( temp->next->type == TYPE_NUMBER ){
				iter = malloc( sizeof( iterator_t ));
				iter->procedure = shared_aquire( temp->data );
				iter->counter = 0;
				iter->limit = temp->next->smalldata;

				ret = alloc_token( );
				ret->type = TYPE_ITERATOR;
				ret->flags = T_FLAG_HAS_SHARED;
				ret->data = shared_new( iter, iterator_free );
				DEBUGP( "[%s] Returning new iterator with limit %d at %p (%p)\n",
					__func__, iter->limit, ret->data, iter );
			}
		}
	}

	return ret;
}

token_t *builtin_iterator_access( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *temp = frame->expr->next;
	token_t *proc;
	token_t *num;
	iterator_t *iter;
	stack_frame_t *temp_frame;
	stack_frame_t *foo_frame;

	DEBUGP( "[%s] Got here\n", __func__ );

	if ( frame->ntokens == 2 ){
		if ( temp->type == TYPE_ITERATOR ){
			iter = shared_get( temp->data );
			frame = calloc( 1, sizeof( stack_frame_t ));

			num = alloc_token( );
			num->type = TYPE_NUMBER;
			num->smalldata = iter->counter;

			proc = alloc_token( );
			proc->type = TYPE_PROCEDURE;
			proc->data = shared_aquire( iter->procedure );
			proc->flags = T_FLAG_HAS_SHARED;
			proc->next = num;

			foo_frame = frame_create( NULL, NULL );
			temp_frame = frame_create( foo_frame, proc );
			eval_loop( temp_frame, NULL );

			ret = foo_frame->expr;
		}
	}

	return ret;
}

token_t *builtin_iterator_next( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *temp = frame->expr->next;
	iterator_t *iter, *other;

	DEBUGP( "[%s] Got here\n", __func__ );

	if ( frame->ntokens == 2 ){
		if ( temp->type == TYPE_ITERATOR ){
			other = shared_get( temp->data );

			if ( !other->limit || other->counter + 1 < other->limit ){
				iter = malloc( sizeof( iterator_t ));
				iter->procedure = shared_aquire( other->procedure );
				iter->counter = other->counter + 1;
				iter->limit = other->limit;

				ret = alloc_token( );
				ret->type = TYPE_ITERATOR;
				ret->flags = T_FLAG_HAS_SHARED;
				ret->data = shared_new( iter, iterator_free );
				DEBUGP( "[%s] Returning new iterator at %p (%p)\n", __func__, ret->data, iter );

			} else {
				ret = alloc_token( );
				ret->type = TYPE_LIST;
				ret->down = NULL;
			}
		}
	}

	return ret;
}
