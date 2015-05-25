#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/files.h>
#include <gojira/parse_debug.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

token_t *ext_proc_token( scheme_func handle ){
	token_t *ret = NULL;

	ret = alloc_token( );

	ret->func = handle;
	ret->type = TYPE_EXTERN_PROC;

	return ret;
}

token_t *builtin_add( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 0;

	ret = alloc_token( );
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	foreach_in_list( move ){
		if ( move->type == TYPE_NUMBER ){
			sum += move->smalldata;

		} else {
			frame->error_call( frame, "[%s] Error: Bad argument type \"%s\"\n",
					__func__, type_str( move->type ));
			break;
		}
	}

	ret->smalldata = sum;

	return ret;
}

void iterator_free( void *ptr ){
	iterator_t *iter = ptr;
	printf( "[%s] Got here, %p\n", __func__, ptr );
	shared_release( iter->procedure );
	free( ptr );
}

token_t *builtin_iterator( stack_frame_t *frame ){
	token_t *temp = frame->expr->next;
	token_t *ret = NULL;
	iterator_t *iter;

	if ( frame->ntokens == 2 ){
		if ( temp->type == TYPE_PROCEDURE ){
			printf( "[%s] Got here, makin an iterator\n", __func__ );
			iter = malloc( sizeof( iterator_t ));
			iter->procedure = shared_aquire( temp->data );
			iter->counter = 0;
			iter->limit = 0;

			ret = alloc_token( );
			ret->type = TYPE_ITERATOR;
			ret->flags = T_FLAG_HAS_SHARED;
			ret->data = shared_new( iter, iterator_free );
		}

	} else if ( frame->ntokens == 3 ) {
		if ( temp->type == TYPE_PROCEDURE ){
			if ( temp->next->type == TYPE_NUMBER ){
				printf( "[%s] Got here, makin an iterator with a limit\n", __func__ );
				iter = malloc( sizeof( iterator_t ));
				iter->procedure = shared_aquire( temp->data );
				iter->counter = 0;
				iter->limit = temp->next->smalldata;

				ret = alloc_token( );
				ret->type = TYPE_ITERATOR;
				ret->flags = T_FLAG_HAS_SHARED;
				ret->data = shared_new( iter, iterator_free );
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

			} else {
				ret = alloc_token( );
				ret->type = TYPE_LIST;
				ret->down = NULL;
			}
		}
	}

	return ret;
}

token_t *builtin_car( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = move;

	if ( move && move->type == TYPE_LIST && move->down ){
		ret = move->down;

    } else if ( move && move->type == TYPE_ITERATOR ){
        ret = builtin_iterator_access( frame );

	} else {
		frame->error_call( frame, "[%s] Bad argument type \"%s\"\n", __func__,
				type_str( move->type ));
	}

	return ret;
}

token_t *builtin_cdr( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = move;

	if ( move && move->type == TYPE_LIST && move->down ){
		ret = alloc_token( );
		ret->type = TYPE_LIST;
		ret->down = move->down->next;

    } else if ( move && move->type == TYPE_ITERATOR ){
        ret = builtin_iterator_next( frame );

	} else {
		frame->error_call( frame, "[%s] Bad argument type \"%s\"\n", __func__,
				type_str( move->type ));
	}

	return ret;
}

token_t *builtin_cons( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *temp;
	token_t *ret = NULL;

	if ( move && move->next ){
		if ( move->next->type == TYPE_LIST ){
			temp = clone_token_tree( move );
			temp->next = move->next->down;

			ret = alloc_token( );
			ret->type = TYPE_LIST;
			ret->down = temp;
			ret->next = NULL;

		} else {
			frame->error_call( frame, "[%s] Bad argument type \"%s\", expected list\n", __func__, type_str( move->next->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Expected 2 arguments, have %d\n", __func__, tokens_length( move ));
	}

	return ret;
}

token_t *builtin_divide( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 1;

	ret = alloc_token( );
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	if ( move ){
		sum = move->smalldata;
		move = move->next;

		foreach_in_list( move ){
			if ( move->type == TYPE_NUMBER ){
				sum /= move->smalldata;

			} else {
				frame->error_call( frame, "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
				break;
			}
		}
	}

	ret->smalldata = sum;

	return ret;
}

token_t *builtin_display( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;

	ret = alloc_token( );
	ret->type = TYPE_NULL;

	move = frame->expr->next;
	if ( move ){
		print_token( move );
	}

	return ret;
}

token_t *builtin_equal( stack_frame_t *frame ){
	token_t *ret;
	bool val = false;

	token_t *op1, *op2;

	ret = alloc_token( );
	ret->type = TYPE_BOOLEAN;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if (( op1->type == TYPE_SYMBOL && op2->type == TYPE_SYMBOL ) ||
		    ( op1->type == TYPE_STRING && op2->type == TYPE_STRING )){
			val = strcmp( op1->data, op2->data ) == 0;

		} else {
			val =	( op1->type      == op2->type      ) &&
					( op1->smalldata == op2->smalldata ) &&
					( op1->down      == op2->down      );
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"eq?\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *builtin_greaterthan( stack_frame_t *frame ){
	token_t *ret;
	bool val = false;

	token_t *op1, *op2;

	ret = alloc_token( );
	ret->type = TYPE_BOOLEAN;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		val =	( op1->type          == op2->type      ) &&
				((int)op1->smalldata > (int)op2->smalldata );

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \">\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *builtin_intern_set( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	token_t *var;

	ret = alloc_token( );
	ret->type = TYPE_NULL;

	move = frame->expr->next;

	if ( move ){
		if ( move->type == TYPE_SYMBOL ){
			var = move;

			if ( move->next ){
				frame_add_var( frame->last, var->data, move->next, NO_RECURSE );
			} else {
				frame->error_call( frame, "[%s] Error: Invalid set, expected value after symbol\n", __func__ );
			}

		} else {
			frame->error_call( frame, "[%s] Error: expected symbol, but got \"%s\"\n", __func__, type_str( move->type ));
		}
	} else {
		frame->error_call( frame, "[%s] Error: expected symbol, but have no arguments.\n", __func__ );
	}

	return ret;
}

token_t *builtin_intern_set_global( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	token_t *var;
	st_frame_t *first;

	ret = alloc_token( );
	ret->type = TYPE_NULL;

	move = frame->expr->next;

	if ( move ){
		if ( move->type == TYPE_SYMBOL ){
			var = move;

			if ( move->next ){
				for ( first = frame; first->last; first = first->last );

				frame_add_var( first, var->data, move->next, NO_RECURSE );
			} else {
				frame->error_call( frame, "[%s] Error: Invalid set, expected value after symbol\n", __func__ );
			}

		} else {
			frame->error_call( frame, "[%s] Error: expected symbol, but got \"%s\"\n", __func__, type_str( move->type ));
		}
	} else {
		frame->error_call( frame, "[%s] Error: expected symbol, but have no arguments.\n", __func__ );
	}

	return ret;
}

token_t *builtin_is_list( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;

	if ( frame->ntokens - 1 == 1 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;

		move = frame->expr->next;
		ret->smalldata = move->type == TYPE_LIST;

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"list?\"\n", __func__ );
	}

	return ret;
}

token_t *builtin_is_null( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	bool val = false;

	ret = alloc_token( );
	ret->type = TYPE_BOOLEAN;

	if ( frame->ntokens - 1 == 1 ){
		move = frame->expr->next;

		val = move->type == TYPE_LIST && move->down == NULL;

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"null?\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *builtin_lessthan( stack_frame_t *frame ){
	token_t *ret;
	bool val = false;

	token_t *op1, *op2;

	ret = alloc_token( );
	ret->type = TYPE_BOOLEAN;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		val =	( op1->type          == op2->type      ) &&
				((int)op1->smalldata < (int)op2->smalldata );

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"<\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *builtin_list( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = NULL;

	ret = alloc_token( );
	ret->type = TYPE_LIST;
	ret->down = move;

	return ret;
}

token_t *builtin_modulo( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if ( op1->type == TYPE_NUMBER && op2->type == TYPE_NUMBER ){
			ret = alloc_token( );
			ret->type = TYPE_NUMBER;

			ret->smalldata = op1->smalldata % op2->smalldata;

		} else {
			frame->error_call( frame, "[%s] Error: Expected number, but have \"%s\" and \"%s\"", __func__,
					type_str( op1->type  ), type_str( op2->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"<\"\n", __func__ );
	}

	return ret;
}

token_t *builtin_multiply( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 1;

	ret = alloc_token( );
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	foreach_in_list( move ){
		if ( move->type == TYPE_NUMBER ){
			sum *= move->smalldata;

		} else {
			frame->error_call( frame, "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
			break;
		}
	}

	ret->smalldata = sum;

	return ret;
}

token_t *builtin_newline( stack_frame_t *frame ){
	token_t *ret;

	ret = alloc_token( );
	ret->type = TYPE_NULL;

	putchar( '\n' );

	return ret;
}

token_t *builtin_return_first( stack_frame_t *frame ){
	return frame->expr->next;
}

token_t *builtin_return_last( stack_frame_t *frame ){
	return frame->end;
}

token_t *builtin_stacktrace( stack_frame_t *frame ){
	token_t *ret;

	ret = alloc_token( );
	ret->type = TYPE_NULL;

	stack_trace( frame );

	return ret;
}

token_t *builtin_sleep( stack_frame_t *frame ){
	token_t *ret;

	ret = alloc_token( );
	ret->type = TYPE_NULL;
	sleep( 1 );

	return ret;
}

token_t *builtin_subtract( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 0;

	ret = alloc_token( );
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	if ( move ){
		sum = move->smalldata;
		move = move->next;

		foreach_in_list( move ){
			if ( move->type == TYPE_NUMBER ){
				sum -= move->smalldata;

			} else {
				frame->error_call( frame, "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
				break;
			}
		}
	}

	ret->smalldata = sum;

	return ret;
}

token_t *builtin_true( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		ret = frame->expr->next;

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments, but got %d\n",
				__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_false( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		ret = frame->expr->next->next;

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments, but got %d\n",
				__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_load_global_file( stack_frame_t *frame ){
	token_t *ret = NULL;
	stack_frame_t *tempframe;
	token_t *oldptr;
	bool eval_return;

	if ( frame->ntokens == 2 ){
		if ( frame->expr->next->type == TYPE_STRING ){
			tempframe = frame;
			for ( ; tempframe->last; tempframe = tempframe->last );

			oldptr = tempframe->ptr;
			eval_return = evaluate_file( tempframe, frame->expr->next->data );
			tempframe->ptr = oldptr;
			//frame->status = EVAL_STATUS_RUNNING;

			ret = alloc_token( );
			ret->type      = TYPE_BOOLEAN;
			ret->smalldata = eval_return;

		} else {
			// bad type
			frame->error_call( frame, "[%s] Error: Expected a string as the pathname, but got a %s\n",
					__func__, type_str( frame->expr->next->type ));
		}

	} else {
		// not enough tokens
		frame->error_call( frame, "[%s] Error: Expected 1 argument, but have %d\n",
				__func__, frame->ntokens - 1 );
	}

	return ret;
}
