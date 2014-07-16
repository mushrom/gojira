#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

token_t *ext_proc_token( scheme_func handle ){
	token_t *ret = NULL;
	ext_proc_t *ext;

	ret = calloc( 1, sizeof( token_t ));
	ext = calloc( 1, sizeof( ext_proc_t ));

	ext->handler = handle;
	ret->data = ext;
	ret->type = TYPE_EXTERN_PROC;

	return ret;
}

token_t *builtin_equal( stack_frame_t *frame ){
	token_t *ret;
	bool val = false;

	token_t *op1, *op2;

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_BOOLEAN;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		val =	( op1->type      == op2->type      ) &&
				( op1->smalldata == op2->smalldata ) &&
				( op1->down      == op2->down      );

	} else {
		printf( "[%s] Error: Expected 2 arguments to \"eq?\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *builtin_add( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 0;

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	foreach_in_list( move ){
		if ( move->type == TYPE_NUMBER ){
			sum += move->smalldata;

		} else {
			printf( "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
			break;
		}
	}

	ret->smalldata = sum;

	return ret;
}

token_t *builtin_multiply( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 1;

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	foreach_in_list( move ){
		if ( move->type == TYPE_NUMBER ){
			sum *= move->smalldata;

		} else {
			printf( "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
			break;
		}
	}

	ret->smalldata = sum;

	return ret;
}

token_t *builtin_subtract( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 0;

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	if ( move ){
		sum = move->smalldata;
		move = move->next;

		foreach_in_list( move ){
			if ( move->type == TYPE_NUMBER ){
				sum -= move->smalldata;

			} else {
				printf( "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
				break;
			}
		}
	}

	ret->smalldata = sum;

	return ret;
}

token_t *builtin_divide( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 1;

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	if ( move ){
		sum = move->smalldata;
		move = move->next;

		foreach_in_list( move ){
			if ( move->type == TYPE_NUMBER ){
				sum /= move->smalldata;

			} else {
				printf( "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
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

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_NULL;

	move = frame->expr->next;
	if ( move ){
		switch ( move->type ){
			case TYPE_NUMBER:
				printf( "%d", move->smalldata );
				break;
			case TYPE_BOOLEAN:
				printf( "#%c", (move->smalldata == true)? 't' : 'f' );
				break;
			default:
				printf( "#<%s>", type_str( move->type ));
				break;
		}
	}

	return ret;
}

token_t *builtin_newline( stack_frame_t *frame ){
	token_t *ret;

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_NULL;

	putchar( '\n' );

	return ret;
}

token_t *builtin_stacktrace( stack_frame_t *frame ){
	token_t *ret;

	ret = calloc( 1, sizeof( token_t ));
	ret->type = TYPE_NULL;

	stack_trace( frame );

	return ret;
}
