#include <gojira/runtime/builtin.h>
#include <gojira/libs/numbers.h>
#include <gojira/parse_debug.h>
#include <time.h>

token_t *builtin_add( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	number_t sum = as_int_number( 0 ); 
	bool error = false;

	move = frame->expr->next;
	foreach_in_list( move ){
		if ( has_number_type( move )) {
			sum = number_add( sum, move->number );

		} else {
			error = true;
			FRAME_ERROR_ARGTYPE( frame, "number", move->type );
			break;
		}
	}

	if ( !error ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = sum.type;
		ret->number = sum;
	}

	return ret;
}

token_t *builtin_divide( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	number_t sum = as_int_number( 1 );
	bool error = false;

	move = frame->expr->next;

	if ( move ){
		if ( has_number_type( move )){
			sum = move->number;
			move = move->next;

			foreach_in_list( move ){
				if ( has_number_type( move )){
					sum = number_div( sum, move->number );

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
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = sum.type;
		ret->number = sum;
	}

	return ret;
}

token_t *builtin_floor( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *move = frame->expr->next;

		if ( has_number_type( move )){
			number_t temp;

			temp = number_mul( as_real_number(1.0), move->number );

			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NUMBER;
			ret->number = as_int_number( temp.real );

		} else {
			FRAME_ERROR_ARGTYPE( frame, "number", move->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_multiply( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	number_t sum = as_int_number( 1 );
	bool error = false;

	move = frame->expr->next;

	if ( move ){
		foreach_in_list( move ){
			if ( has_number_type( move )){ 
				sum = number_mul( sum, move->number );

			} else {
				error = true;
				FRAME_ERROR_ARGTYPE( frame, "number", move->type );
				break;
			}
		}

		if ( !error ){
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = sum.type;
			ret->number = sum;
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_modulo( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;

	if ( frame->ntokens == 3 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if ( op1->type == TYPE_NUMBER && op2->type == TYPE_NUMBER ){
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NUMBER;
			ret->number.s_int = op1->number.s_int % op2->number.s_int;
			ret->number.type = TYPE_NUMBER;

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

token_t *builtin_subtract( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	number_t sum = as_int_number( 0 );
	bool error = false;

	move = frame->expr->next;

	if ( move ){
		sum = move->number;
		move = move->next;

		foreach_in_list( move ){
			if ( has_number_type( move )) {
				sum = number_sub( sum, move->number );

			} else {
				FRAME_ERROR_ARGTYPE( frame, "number", move->type );
				error = true;
				break;
			}
		}
	}

	if ( !error ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = sum.type;
		ret->number = sum;
	}

	return ret;
}

token_t *builtin_random_int( stack_frame_t *frame ){
	static bool seeded = false;
	token_t *ret = NULL;

	if ( !seeded ){
		srand( time( NULL ));
		seeded = true;
	}

	ret = gc_alloc_token( get_current_gc( frame ));
	ret->type = TYPE_NUMBER;
	ret->number = as_int_number( rand( ));

	return ret;
}

token_t *builtin_is_number( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		ret->boolean = has_number_type( frame->expr->next );

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_is_integer( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		ret->boolean = frame->expr->next->type == TYPE_NUMBER;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_is_rational( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *move = frame->expr->next;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		ret->boolean =
			   move->type == TYPE_RATIONAL
			|| move->type == TYPE_NUMBER;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_is_real( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *move = frame->expr->next;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		ret->boolean =
			   move->type == TYPE_REAL
			|| move->type == TYPE_RATIONAL
			|| move->type == TYPE_NUMBER;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_char_to_integer( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *arg = frame->expr->next;

		if ( arg->type == TYPE_CHAR ){
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NUMBER;
			ret->number = as_int_number( arg->character );

		} else {
			FRAME_ERROR_ARGTYPE( frame, "character", arg->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_integer_to_char( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *arg = frame->expr->next;

		if ( arg->type == TYPE_NUMBER ){
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_CHAR;
			ret->character = arg->number.u_int;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "integer", arg->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}
