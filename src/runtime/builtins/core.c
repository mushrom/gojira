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
	token_t *ret = NULL;
	token_t *move;
	int sum = 0;
	bool error = false;

	move = frame->expr->next;
	foreach_in_list( move ){
		if ( move->type == TYPE_NUMBER ){
			sum += move->smalldata;

		} else {
			error = true;
			FRAME_ERROR_ARGTYPE( frame, "number", move->type );
			break;
		}
	}

	if ( !error ){
		ret = alloc_token( );
		ret->type = TYPE_NUMBER;
		ret->smalldata = sum;
	}

	return ret;
}

token_t *builtin_car( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = NULL;

	if ( move && move->type == TYPE_LIST && move->down ){
		ret = move->down;

    } else if ( move && move->type == TYPE_ITERATOR ){
        ret = builtin_iterator_access( frame );

	} else {
		FRAME_ERROR_ARGTYPE( frame, "list", move->type );
	}

	return ret;
}

token_t *builtin_cdr( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = NULL;

	if ( move && move->type == TYPE_LIST && move->down ){
		ret = alloc_token( );
		ret->type = TYPE_LIST;
		ret->down = move->down->next;

    } else if ( move && move->type == TYPE_ITERATOR ){
        ret = builtin_iterator_next( frame );

	} else {
		FRAME_ERROR_ARGTYPE( frame, "list", move->type );
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
			FRAME_ERROR_ARGTYPE( frame, "list", move->next->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_divide( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	int sum = 1;
	bool error = false;

	move = frame->expr->next;

	if ( move ){
		if ( move->type == TYPE_NUMBER ){
			sum = move->smalldata;
			move = move->next;

			foreach_in_list( move ){
				if ( move->type == TYPE_NUMBER ){
					sum /= move->smalldata;

				} else {
					FRAME_ERROR_ARGTYPE( frame, "number", move->type );
					error = true;
					break;
				}
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "number", move->type );
			error = true;
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
		error = true;
	}

	if ( !error ){
		ret = alloc_token( );
		ret->type = TYPE_NUMBER;
		ret->smalldata = sum;
	}

	return ret;
}

token_t *builtin_equal( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;
	bool val = false;

	if ( frame->ntokens == 3 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;

		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if ( op1->type == op2->type ){
			switch( op1->type ){
				case TYPE_STRING:
				case TYPE_SYMBOL:
					val = strcmp( shared_get( op1->data ),
					              shared_get( op2->data )) == 0;
					break;

				default:
					val = ( op1->smalldata == op2->smalldata )
					   && ( op1->down      == op2->down );
					break;
			}

		} // default value of 'val' is false, so just continue

		ret->smalldata = val;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_greaterthan( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;

	if ( frame->ntokens == 3 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;
		ret->smalldata = ( op1->type          == op2->type )
		              && ((int)op1->smalldata > (int)op2->smalldata );

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_intern_set( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	token_t *temp;
	variable_t *var;

	if ( frame->ntokens == 3 ){
		bool error = false;
		move = frame->expr->next;

		if ( move->type == TYPE_SYMBOL ){
			char *varname = shared_get( move->data );
			frame_add_var( frame->last, varname, move->next, NO_RECURSE );

		} else if ( move->type == TYPE_VARIABLE_REF ){
			var = shared_get( move->data );
			temp = var->token;
			var->token = clone_token( move->next );
			frame_register_tokens( frame, temp );

		} else {
			FRAME_ERROR( frame,
				"expected symbol or variable reference, but have %s",
				type_str( move->type ));
		}

		if ( !error ){
			ret = alloc_token( );
			ret->type = TYPE_NULL;
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_intern_set_global( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	st_frame_t *first;

	move = frame->expr->next;

	if ( frame->ntokens == 3 ){
		if ( move->type == TYPE_SYMBOL ){
			char *varname = shared_get( move->data );

			for ( first = frame; first->last; first = first->last );
			frame_add_var( first, varname, move->next, NO_RECURSE );

			ret = alloc_token( );
			ret->type = TYPE_NULL;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "symbol", move->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_is_list( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;

	if ( frame->ntokens == 2 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;

		move = frame->expr->next;
		ret->smalldata = move->type == TYPE_LIST;

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

token_t *builtin_is_null( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *tok;

	if ( frame->ntokens == 2 ){
		tok = frame->expr->next;

		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;
		ret->smalldata = tok->type == TYPE_LIST && tok->down == NULL;

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

token_t *builtin_lessthan( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;

	if ( frame->ntokens == 3 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;

		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		ret->smalldata = ( op1->type          == op2->type )
			          && ((int)op1->smalldata < (int)op2->smalldata );

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_list( stack_frame_t *frame ){
	token_t *ret = NULL;

	ret = alloc_token( );
	ret->type = TYPE_LIST;
	ret->down = frame->expr->next;

	return ret;
}

token_t *builtin_modulo( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;

	if ( frame->ntokens == 3 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if ( op1->type == TYPE_NUMBER && op2->type == TYPE_NUMBER ){
			ret = alloc_token( );
			ret->type = TYPE_NUMBER;
			ret->smalldata = op1->smalldata % op2->smalldata;

		} else {
			FRAME_ERROR( frame,
				"expected number, but have %s and %s",
				type_str( op1->type  ), type_str( op2->type ));
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_multiply( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	int sum = 1;
	bool error = false;

	move = frame->expr->next;

	if ( move ){
		foreach_in_list( move ){
			if ( move->type == TYPE_NUMBER ){
				sum *= move->smalldata;

			} else {
				error = true;
				FRAME_ERROR_ARGTYPE( frame, "number", move->type );
				break;
			}
		}

		if ( !error ){
			ret = alloc_token( );
			ret->type = TYPE_NUMBER;
			ret->smalldata = sum;
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

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
	token_t *ret = NULL;
	token_t *move;
	int sum = 0;

	move = frame->expr->next;

	if ( move ){
		ret = alloc_token( );
		ret->type = TYPE_NUMBER;

		sum = move->smalldata;
		move = move->next;

		foreach_in_list( move ){
			if ( move->type == TYPE_NUMBER ){
				sum -= move->smalldata;

			} else {
				FRAME_ERROR_ARGTYPE( frame, "number", move->type );
				break;
			}
		}

		ret->smalldata = sum;
	}

	return ret;
}

token_t *builtin_true( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		ret = frame->expr->next;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_false( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		ret = frame->expr->next->next;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
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
			char *fname = shared_get( frame->expr->next->data );
			tempframe = frame;
			for ( ; tempframe->last; tempframe = tempframe->last );

			oldptr = tempframe->ptr;
			eval_return = evaluate_file( tempframe, fname );
			tempframe->ptr = oldptr;

			ret = alloc_token( );
			ret->type      = TYPE_BOOLEAN;
			ret->smalldata = eval_return;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "string", frame->expr->next->type );
		}

	} else {
		// not enough tokens
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}
