#include <gojira/runtime/builtin.h>
#include <stdlib.h>

token_t *builtin_system( stack_frame_t *frame ){
	token_t *ret = NULL;
		
	if ( frame->ntokens == 2 ){
		token_t *cmd = frame->expr->next;
		
		if ( cmd->type == TYPE_STRING ){
			const char *str = shared_get( cmd->data );

			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NUMBER;
			ret->number = as_int_number( system( str ));

		} else {
			FRAME_ERROR_ARGTYPE( frame, "string", cmd->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}
