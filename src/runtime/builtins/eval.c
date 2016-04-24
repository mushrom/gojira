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
			frame->ptr = move->down;
			frame->flags |= RUNTIME_FLAG_CONTINUE;
			frame->expr = NULL;

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

		if ( arglist->type == TYPE_LIST ){
			token_t *code = gc_clone_token( get_current_gc( frame ), func );

			code->next = arglist->down;
			frame->expr = code;
			frame->flags |= RUNTIME_FLAG_CONTINUE;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "list", func->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}
	
	return ret;
}
