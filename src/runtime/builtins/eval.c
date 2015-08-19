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

token_t *builtin_eval( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 2 ){
		token_t *move = frame->expr->next;

		if ( move->type == TYPE_LIST ){
			stack_frame_t *tempframe = frame_create( frame, move->down );
			tempframe->flags |= RUNTIME_FLAG_BREAK;

			eval_loop( tempframe );
			ret = frame->end;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "list", move->type );
		}
		
	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}
	
	return ret;
}

token_t *builtin_apply( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		token_t *func = frame->expr->next;
		token_t *arglist = frame->expr->next->next;

		if ( func->type == TYPE_EXTERN_PROC
			|| func->type == TYPE_PROCEDURE
			|| func->type == TYPE_BOOLEAN
			|| func->type == TYPE_SYNTAX
			|| func->type == TYPE_HASHMAP )
		{
			if ( arglist->type == TYPE_LIST ){
				token_t *code = clone_token( func );
				stack_frame_t *tempframe;

				code->next = arglist->down;
				tempframe = frame_create( frame, code );
				tempframe->flags |= RUNTIME_FLAG_BREAK;

				eval_loop( tempframe );
				ret = tempframe->value;

				free_token( code );

			} else {
				FRAME_ERROR_ARGTYPE( frame, "list", func->type );
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, "procedure", func->type );
		}
		
	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}
	
	return ret;
}
