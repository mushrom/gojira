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

token_t *builtin_char_to_string( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	char *str = NULL;
	unsigned len;
	unsigned i;
	bool all_char = true;

	if ( frame->ntokens == 2 ){
		if ( frame->expr->next->type == TYPE_LIST ){
			move = frame->expr->next->down;
			len = tokens_length( move );
			str = malloc( sizeof( char[len + 1] ));

			i = 0;
			foreach_in_list( move ){
				if ( move->type == TYPE_CHAR ){
					str[i] = move->smalldata;
					i++;

				} else {
					all_char = false;
					break;
				}
			}

			if ( all_char ){
				str[i] = 0;
				ret = alloc_token( );
				ret->type = TYPE_STRING;
				ret->data = str;
			} else {
				free( str );
			}
		} else {
			frame->error_call( frame, "[%s] Expected list, but have %s\n",
					__func__, type_str( frame->expr->next->type ));
		}
	} else {
		frame->error_call( frame, "[%s] Expected 1 argument, but have %d\n",
				__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_is_string( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;
		ret->smalldata = frame->expr->next->type == TYPE_STRING;

	} else {
		frame->error_call( frame, "[%s] Need moar tokenz\n", __func__ );
	}

	return ret;
}

token_t *builtin_string_ref( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		if ( frame->expr->next->type == TYPE_STRING ){
			if ( frame->expr->next->next->type == TYPE_NUMBER ){
				char *str = frame->expr->next->data;
				unsigned n = frame->expr->next->next->smalldata;
				unsigned len = strlen( str );

				if ( n < len ){
					ret = alloc_token( );
					ret->type = TYPE_CHAR;
					ret->smalldata = str[n];

				} else {
					frame->error_call( frame,
						"[%s] Error: Index too large, string length is %u but index is %u\n",
						__func__, len, n );
				}

			} else {
				frame->error_call( frame,
					"[%s] Expected number, but have %s\n",
					__func__, type_str( frame->expr->next->next->type ));
			}

		} else {
			frame->error_call( frame,
				"[%s] Expected string, but have %s\n",
				__func__, type_str( frame->expr->next->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Need moar tokenz\n", __func__ );
	}

	return ret;
}
