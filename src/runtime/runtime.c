#include <gojira/runtime/runtime.h>
#include <gojira/runtime/frame.h>
#include <gojira/runtime/syntax.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/garbage.h>
#include <gojira/parse_debug.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

typedef enum {
	EVAL_STATUS_NONE,
	EVAL_STATUS_ERROR,
	EVAL_STATUS_RUNNING,
} eval_ret_t;

eval_ret_t eval_step( stack_frame_t *base, stack_frame_t **frame, token_t *tokens ){
	eval_ret_t ret = EVAL_STATUS_RUNNING;

	// Evaluate sub-expressions
	if ( (*frame)->ptr ){
		if ( eval_frame_subexpr( frame, base ) == true )
			ret = EVAL_STATUS_ERROR;

	// Evaluation finished, apply function
	} else {
		if ( (*frame)->last ){
			if ( eval_frame_expr( frame, base ) == true )
				ret = EVAL_STATUS_ERROR;

		} else {
			ret = EVAL_STATUS_NONE;
		}
	}

	return ret;
}

token_t *eval_loop( stack_frame_t *base, token_t *tokens ){
	stack_frame_t *frame = base;
	token_t *ret = tokens;

	eval_ret_t status = EVAL_STATUS_RUNNING;

	while ( status == EVAL_STATUS_RUNNING ){
		status = eval_step( base, &frame, tokens );
	}

	ret = base->expr;

	return ret;
}

token_t *eval_loop_timed( stack_frame_t *base, token_t *tokens, unsigned limit ){
	stack_frame_t *frame = base;
	token_t *ret = tokens;

	eval_ret_t status = EVAL_STATUS_RUNNING;
	unsigned cycles = 0;

	while ( status == EVAL_STATUS_RUNNING && cycles < limit ){
		status = eval_step( base, &frame, tokens );
		cycles++;
	}

	ret = base->expr;

	return ret;
}

bool eval_frame_subexpr( stack_frame_t **frame_ret, stack_frame_t *first ){
	bool ret = false;
	stack_frame_t *frame = *frame_ret;
	token_t *move;

	switch ( frame->ptr->type ){
		case TYPE_LIST:
			move = frame->ptr;
			frame->ptr = frame->ptr->next;

			if ( move->down == NULL ){
				frame->error_call( frame, "[%s] Error: Empty expression\n", __func__ );

				//stack_trace( frame );
				ret = true;
				break;
			}

			frame = *frame_ret = frame_create( frame, move->down );
			break;

		case TYPE_SYMBOL:
			if ( *(char *)frame->ptr->data == ':' ){
				frame_add_token( frame, frame->ptr );
				frame->ptr = frame->ptr->next;

			} else {
				move = frame_find_var( frame, frame->ptr->data, RECURSE );

				if ( move ){
					frame_add_token( frame, move );

					if ( move->type != TYPE_SYNTAX ){
						frame->ptr = frame->ptr->next;

					} else {
						for ( frame->ptr = frame->ptr->next; frame->ptr; frame->ptr = frame->ptr->next )
							frame_add_token( frame, frame->ptr );
					}

				} else {
					frame->error_call( frame, "[%s] Error: undefined variable \"%s\"\n",
							__func__, (char *)frame->ptr->data );

					ret = true;
				}
			}

			break;

		case TYPE_VARIABLE_REF:
			{
				shared_t *shr;
				variable_t *var;

				shr = shared_aquire( frame->ptr->data );
				var = shared_get( shr );
				frame_add_token( frame, var->token );

				if ( var->token->type != TYPE_SYNTAX ){
					frame->ptr = frame->ptr->next;

				} else {
					for ( frame->ptr = frame->ptr->next; frame->ptr; frame->ptr = frame->ptr->next )
						frame_add_token( frame, frame->ptr );
				}
			}

			break;

		case TYPE_LAMBDA:
			frame->expr = frame->ptr;
			frame->ptr  = NULL;

			break;

		case TYPE_SYNTAX_RULES:
			move = alloc_token( );
			move->type = TYPE_SYNTAX;
			move->down = frame->ptr;

			frame_add_token_noclone( frame, ext_proc_token( builtin_return_first ));
			frame_add_token( frame, move );

			frame->ptr = NULL;
			break;

		case TYPE_QUOTED_TOKEN:
			frame_add_token( frame, frame->ptr->down );
			frame->ptr = frame->ptr->next;
			break;

		case TYPE_VECTOR:
			move = expand_vector( frame, frame->ptr );

			if ( move )
				frame_add_token_noclone( frame, move );
			else
				ret = true;

			frame->ptr = frame->ptr->next;
			break;

		default:
			frame_add_token( frame, frame->ptr );
			frame->ptr = frame->ptr->next;
			break;
	}

	return ret;
}

bool eval_frame_expr( stack_frame_t **frame_ret, stack_frame_t *first ){
	bool ret = false;
	bool apply = true;
	stack_frame_t *frame = *frame_ret;
	stack_frame_t *temp_frame;
	token_t *foo;
	scheme_func handle;

	frame->status = frame->expr->type;

	switch ( frame->expr->type ){
		case TYPE_EXTERN_PROC:
			//handle = frame->expr->data;
			handle = frame->expr->func;

			if ( handle )
				frame->value = handle( frame );

			if ( !frame->value )
				ret = true;

			break;

		case TYPE_PROCEDURE:
			temp_frame = expand_procedure( frame, frame->expr );
			frame = *frame_ret = temp_frame;

			apply = false;
			break;

		case TYPE_LAMBDA:
			foo = expand_lambda( frame, frame->expr );
			if ( foo ){
				frame->value = foo;

			} else {
				ret = true;
			}
			break;

		case TYPE_SYNTAX:
			foo = expand_syntax_rules( frame, frame->expr );

			if ( foo ){
				if ( foo->type == TYPE_LIST ){
					frame->ptr = foo->down;
					frame->expr = NULL;
					apply = false;

				} else {
					frame->value = foo;
				}

				break;

			} else {
				ret = true;
			}

			break;

		case TYPE_BOOLEAN:
			{
				foo = ext_proc_token( frame->expr->smalldata? builtin_true : builtin_false );
				frame_register_token( frame, foo );

				foo->next = frame->expr->next;
				frame->expr = foo;
				apply = false;
			}

			break;

		default:
			frame->error_call( frame, "[%s] Can't apply \"%s\"\n", __func__, type_str( frame->expr->type ));
			ret = true;
			break;
	}

	if ( apply && !ret ){
		temp_frame = frame->last;

		gc_mark( frame->value );
		frame->heap = gc_sweep( frame->heap );

		frame_add_token_noclone( temp_frame, frame->value );

		frame_free( frame );
		*frame_ret = temp_frame;
	}

	return ret;
}

