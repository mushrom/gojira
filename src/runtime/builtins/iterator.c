#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/parse_debug.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

void iterator_free( void *ptr ){
	iterator_t *iter = ptr;
	//printf( "[%s] Got here, %p\n", __func__, ptr );
	DEBUGP( "[%s] Got here, %p\n", __func__, ptr );
	shared_release( iter->procedure );

	switch ( iter->next_type ){
		case TYPE_PROCEDURE: shared_release( iter->nproc ); break;
		case TYPE_ITERATOR:  shared_release( iter->iter );  break;
		default: break;
	}

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
			iter->next_type = TYPE_NULL;

			ret = alloc_token( );
			ret->type = TYPE_ITERATOR;
			ret->flags = T_FLAG_HAS_SHARED;
			ret->data = shared_new( iter, iterator_free );

			DEBUGP( "[%s] Returning new iterator at %p (%p)\n", __func__, ret->data, iter );
		}

	} else if ( frame->ntokens == 3 ) {
		if ( temp->type == TYPE_PROCEDURE ){
			if (( temp->next->type == TYPE_NUMBER    ) ||
			    ( temp->next->type == TYPE_PROCEDURE ) ||
			    ( temp->next->type == TYPE_ITERATOR  ))
			{
				iter = malloc( sizeof( iterator_t ));
				iter->procedure = shared_aquire( temp->data );
				iter->counter = 0;
				iter->next_type = temp->next->type;
				switch ( iter->next_type ){
					case TYPE_NUMBER:    iter->limit = temp->next->smalldata; break;
					case TYPE_PROCEDURE: iter->nproc = shared_aquire( temp->next->data ); break;
					case TYPE_ITERATOR:  iter->iter  = shared_aquire( temp->next->data ); break;
					default: /* TODO: Error here */ break;
				}
				//iter->limit = temp->next->smalldata;

				ret = alloc_token( );
				ret->type = TYPE_ITERATOR;
				ret->flags = T_FLAG_HAS_SHARED;
				ret->data = shared_new( iter, iterator_free );
				DEBUGP( "[%s] Returning new iterator of type %s at %p (%p)\n",
					__func__, type_str( iter->next_type ), ret->data, iter );
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
			eval_loop( temp_frame );

			gc_mark( foo_frame->expr );
			foo_frame->heap = gc_sweep( foo_frame->heap );
			frame_free( foo_frame );

			free_token( proc );
			free_token( num );
			// FIXME: this is a bug right?
			frame_free( frame );

			ret = foo_frame->expr;
		}
	}

	return ret;
}

token_t *builtin_iterator_next( stack_frame_t *frame ){ token_t *ret = NULL;
	token_t *temp = frame->expr->next;
	iterator_t *iter, *other;

	DEBUGP( "[%s] Got here\n", __func__ );

	if ( frame->ntokens == 2 ){
		if ( temp->type == TYPE_ITERATOR ){
			other = shared_get( temp->data );

			switch ( other->next_type ){
				case TYPE_NUMBER:
					if (( other->limit && other->counter + 1 < other->limit ) == false ){
						ret = alloc_token( );
						ret->type = TYPE_LIST;
						ret->down = NULL;
						break;
					}

				case TYPE_NULL:
					iter = malloc( sizeof( iterator_t ));
					iter->procedure = shared_aquire( other->procedure );
					iter->counter = other->counter + 1;
					iter->limit = other->limit;
					iter->next_type = other->next_type;

					ret = alloc_token( );
					ret->type = TYPE_ITERATOR;
					ret->flags = T_FLAG_HAS_SHARED;
					ret->data = shared_new( iter, iterator_free );
					DEBUGP( "[%s] Returning new iterator at %p (%p)\n", __func__, ret->data, iter );
					break;

				case TYPE_ITERATOR:
					ret = alloc_token( );
					ret->type = TYPE_ITERATOR;
					ret->flags = T_FLAG_HAS_SHARED;
					ret->data = shared_aquire( other->iter );
					DEBUGP( "[%s] Returning older iterator at %p (%p)\n",
						__func__, ret->data, other->iter );
					break;

				case TYPE_PROCEDURE:
					{
						token_t *num;
						token_t *proc;
						token_t *cur_iter;
						stack_frame_t *temp_frame;
						stack_frame_t *foo_frame;

						iter = shared_get( temp->data );
						frame = calloc( 1, sizeof( stack_frame_t ));

						num = alloc_token( );
						num->type = TYPE_NUMBER;
						num->smalldata = iter->counter;

						cur_iter = alloc_token( );
						cur_iter->type = TYPE_ITERATOR;
						cur_iter->data = shared_aquire( temp->data );
						cur_iter->flags = T_FLAG_HAS_SHARED;
						cur_iter->next = num;

						proc = alloc_token( );
						proc->type = TYPE_PROCEDURE;
						proc->data = shared_aquire( iter->nproc );
						proc->flags = T_FLAG_HAS_SHARED;
						proc->next = cur_iter;

						foo_frame = frame_create( NULL, NULL );
						temp_frame = frame_create( foo_frame, proc );
						eval_loop( temp_frame );

						gc_mark( foo_frame->expr );
						gc_sweep( foo_frame->heap );
						frame_free( foo_frame );

						ret = foo_frame->expr;

						if ( ret && ret->type == TYPE_ITERATOR ){
							DEBUGP( "[%s] Got here\n", __func__ );
							token_t *foo;
							iterator_t *temp_iter;

							temp_iter = shared_get( ret->data );

							iter = malloc( sizeof( iterator_t ));
							iter->procedure = shared_aquire( temp_iter->procedure );
							iter->counter = temp_iter->counter + 1;
							iter->next_type = temp_iter->next_type;

							switch ( iter->next_type ){
								case TYPE_NUMBER:    iter->limit = temp_iter->limit; break;
								case TYPE_PROCEDURE: iter->nproc = shared_aquire( temp_iter->nproc ); break;
								case TYPE_ITERATOR:  iter->iter  = shared_aquire( temp_iter->iter ); break;
								default:             iter->limit = 0; break;
							}

							foo = alloc_token( );
							foo->type = TYPE_ITERATOR;
							foo->flags = T_FLAG_HAS_SHARED;
							foo->data = shared_new( iter, iterator_free );

							free_tokens( ret );

							ret = foo;
						}
					}

					break;

				default:
					break;
			}

			/*
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
			*/
		}
	}

	return ret;
}
