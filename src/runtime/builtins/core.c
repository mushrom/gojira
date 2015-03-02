#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

token_t *ext_proc_token( scheme_func handle ){
	token_t *ret = NULL;

	//ret = calloc( 1, sizeof( token_t ));
	ret = alloc_token( );

	ret->data = handle;
	ret->type = TYPE_EXTERN_PROC;

	return ret;
}

token_t *builtin_add( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 0;

	//ret = calloc( 1, sizeof( token_t ));
	ret = alloc_token( );
	ret->type = TYPE_NUMBER;

	move = frame->expr->next;
	foreach_in_list( move ){
		if ( move->type == TYPE_NUMBER ){
			sum += move->smalldata;

		} else {
			frame->error_call( frame, "[%s] Error: Bad argument type \"%s\"\n",
					__func__, type_str( move->type ));
			//printf( "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
			break;
		}
	}

	ret->smalldata = sum;

	return ret;
}

token_t *builtin_car( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = move;

	if ( move && move->type == TYPE_LIST && move->down ){
		ret = move->down;

	} else {
		frame->error_call( frame, "[%s] Bad argument type \"%s\"\n", __func__,
				type_str( move->type ));
		/*
		printf( "[%s] Bad argument type \"%s\"\n", __func__, type_str( move->type ));
		stack_trace( frame );
		*/
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

	} else {
		frame->error_call( frame, "[%s] Bad argument type \"%s\"\n", __func__,
				type_str( move->type ));
		/*
		printf( "[%s] Bad argument type \"%s\"\n", __func__, type_str( move->type ));
		stack_trace( frame );
		*/
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
			/*
			printf( "[%s] Bad argument type \"%s\", expected list\n", __func__, type_str( move->next->type ));
			stack_trace( frame );
			*/
		}

	} else {
		frame->error_call( frame, "[%s] Expected 2 arguments, have %d\n", __func__, tokens_length( move ));
		/*
			printf( "[%s] Expected 2 arguments, have %d\n", __func__, tokens_length( move ));
			stack_trace( frame );
		*/
	}

	return ret;
}

token_t *builtin_divide( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	int sum = 1;

	//ret = calloc( 1, sizeof( token_t ));
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
				//printf( "[%s] Error: Bad argument type \"%s\"\n", __func__, type_str( move->type ));
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

	//ret = calloc( 1, sizeof( token_t ));
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

	//ret = calloc( 1, sizeof( token_t ));
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
		//printf( "[%s] Error: Expected 2 arguments to \"eq?\"\n", __func__ );
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"eq?\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *builtin_greaterthan( stack_frame_t *frame ){
	token_t *ret;
	bool val = false;

	token_t *op1, *op2;

	//ret = calloc( 1, sizeof( token_t ));
	ret = alloc_token( );
	ret->type = TYPE_BOOLEAN;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		val =	( op1->type          == op2->type      ) &&
				((int)op1->smalldata > (int)op2->smalldata );

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \">\"\n", __func__ );
		//printf( "[%s] Error: Expected 2 arguments to \">\"\n", __func__ );
	}

	ret->smalldata = val;

	return ret;
}

token_t *builtin_intern_set( stack_frame_t *frame ){
	token_t *ret;
	token_t *move;
	token_t *var;

	//ret = calloc( 1, sizeof( token_t ));
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

	//ret = calloc( 1, sizeof( token_t ));
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

	//ret = calloc( 1, sizeof( token_t ));
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

	//ret = calloc( 1, sizeof( token_t ));
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

	//ret = calloc( 1, sizeof( token_t ));
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

	//ret = calloc( 1, sizeof( token_t ));
	ret = alloc_token( );
	ret->type = TYPE_NULL;

	putchar( '\n' );

	return ret;
}

token_t *builtin_read_char( stack_frame_t *frame ){
	token_t *ret = NULL;

	ret = alloc_token( );
	ret->type = TYPE_CHAR;
	ret->smalldata = getchar( );

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

	//ret = calloc( 1, sizeof( token_t ));
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

	//ret = calloc( 1, sizeof( token_t ));
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