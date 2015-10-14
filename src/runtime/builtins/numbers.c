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
		ret = alloc_token( );
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
		ret = alloc_token( );
		ret->type = sum.type;
		ret->number = sum;
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
			ret = alloc_token( );
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
			ret = alloc_token( );
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
		ret = alloc_token( );
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

	ret = alloc_token( );
	ret->type = TYPE_NUMBER;
	ret->number = as_int_number( rand( ));

	return ret;
}
