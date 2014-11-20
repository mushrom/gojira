#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

token_t *builtin_string_append( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;
	char *str;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if ( op1->type == TYPE_STRING && op2->type == TYPE_STRING ){
			ret = alloc_token( );
			ret->type = TYPE_STRING;

			size_t len = strlen( op1->data ) + strlen( op2->data );
			str = calloc( 1, sizeof( char[len + 8]));
			strcpy( str, op1->data );
			strcat( str, op2->data );

			ret->data = str;

		} else {
			frame->error_call( frame, "[%s] Error: Expected strings as arguments, but have \"%s\" and \"%s\"", __func__,
					type_str( op1->type  ), type_str( op2->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"string-append\"\n", __func__ );
	}

	return ret;
}
token_t *builtin_string_contains( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;
	char *str;

	if ( frame->ntokens - 1 == 2 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if ( op1->type == TYPE_STRING && op2->type == TYPE_STRING ){
			ret = alloc_token( );

			if (( str = strstr( op1->data, op2->data ))){
				ret->type = TYPE_NUMBER;
				ret->smalldata = (unsigned)((unsigned long)str - (unsigned long)op1->data );

			} else {
				ret->type = TYPE_BOOLEAN;
				ret->smalldata = false;
			}

		} else {
			frame->error_call( frame, "[%s] Error: Expected strings as arguments, but have \"%s\" and \"%s\"", __func__,
					type_str( op1->type  ), type_str( op2->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 2 arguments to \"string-append\"\n", __func__ );
	}

	return ret;
}

token_t *builtin_string_to_symbol( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1;

	if ( frame->ntokens - 1 == 1 ){
		op1 = frame->expr->next;

		if ( op1->type == TYPE_STRING ){
			ret = alloc_token( );
			ret->type = TYPE_SYMBOL;

			ret->data = op1->data;

		} else {
			frame->error_call( frame, "[%s] Error: Expected symbol as argument, but have \"%s\" and \"%s\"", __func__,
					type_str( op1->type  ));
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 1 arguments to \"symbol->string\"\n", __func__ );
	}

	return ret;
}

token_t *builtin_symbol_to_string( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1;

	if ( frame->ntokens - 1 == 1 ){
		op1 = frame->expr->next;

		if ( op1->type == TYPE_SYMBOL ){
			ret = alloc_token( );
			ret->type = TYPE_STRING;

			ret->data = op1->data;

		} else {
			frame->error_call( frame, "[%s] Error: Expected symbol as argument, but have \"%s\" and \"%s\"", __func__,
					type_str( op1->type  ));
		}

	} else {
		frame->error_call( frame, "[%s] Error: Expected 1 arguments to \"symbol->string\"\n", __func__ );
	}

	return ret;
}
