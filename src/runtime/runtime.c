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

eval_ret_t eval_step( stack_frame_t **frame ){
	eval_ret_t ret = EVAL_STATUS_RUNNING;

	// Evaluate sub-expressions
	if ( (*frame)->ptr ){
		if ( eval_frame_subexpr( frame ) == true )
			ret = EVAL_STATUS_ERROR;

	// Evaluation finished, apply function
	} else {
		if ( (*frame)->last ){
			if ( eval_frame_expr( frame ) == true )
				ret = EVAL_STATUS_ERROR;

		} else {
			ret = EVAL_STATUS_NONE;
		}
	}

	return ret;
}

token_t *eval_loop( stack_frame_t *base ){
	stack_frame_t *frame = base;
	token_t *ret = NULL;

	eval_ret_t status = EVAL_STATUS_RUNNING;

	while ( status == EVAL_STATUS_RUNNING ){
		status = eval_step( &frame );
	}

	ret = base->expr;

	return ret;
}

token_t *eval_loop_timed( stack_frame_t *base, unsigned limit ){
	stack_frame_t *frame = base;
	token_t *ret = NULL;

	eval_ret_t status = EVAL_STATUS_RUNNING;
	unsigned cycles = 0;

	while ( status == EVAL_STATUS_RUNNING && cycles < limit ){
		status = eval_step( &frame );
		cycles++;
	}

	ret = base->expr;

	return ret;
}

bool eval_frame_subexpr( stack_frame_t **frame_ret ){
	bool ret = false;
	stack_frame_t *frame = *frame_ret;
	token_t *move;

	switch ( frame->ptr->type ){
		case TYPE_LIST:
			move = frame->ptr;
			frame->ptr = frame->ptr->next;

			if ( move->down == NULL ){
				frame->error_call( frame, "[%s] Error: Empty expression\n", __func__ );

				ret = true;
				break;
			}

			frame = *frame_ret = frame_create( frame, move->down );
			break;

		case TYPE_SYMBOL:
			{
				char *varname = shared_get( frame->ptr->data );

				if ( *varname == ':' ){
					frame_add_token( frame, frame->ptr );
					frame->ptr = frame->ptr->next;

				} else {
					move = frame_find_var( frame, varname, RECURSE );

					if ( move ){
						if ( move->type != TYPE_SYNTAX || frame->expr ){
							frame_add_token( frame, move );
							frame->ptr = frame->ptr->next;

						} else {
							frame->expr = clone_token( move );
							frame_register_one_token( frame, frame->expr );
							frame->expr->next = frame->ptr->next;
							frame->ptr = NULL;
						}

					} else {
						frame->error_call( frame, "[%s] Error: undefined variable \"%s\"\n",
								__func__, varname );

						ret = true;
					}
				}
			}

			break;

		case TYPE_VARIABLE_REF:
			{
				variable_t *var;
				var = shared_get( frame->ptr->data );

				if ( var->token->type != TYPE_SYNTAX || frame->expr ){
					frame_add_token( frame, var->token );
					frame->ptr = frame->ptr->next;

				} else {
					frame->expr = clone_token( var->token );
					frame_register_one_token( frame, frame->expr );
					frame->expr->next = frame->ptr->next;
					frame->ptr = NULL;
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

bool eval_frame_expr( stack_frame_t **frame_ret ){
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
			if ( frame->last->ptr == NULL && frame->last->status == TYPE_PROCEDURE ){
				temp_frame = expand_procedure( frame->last, frame->expr );
				gc_mark( frame->expr );
				gc_sweep( frame->heap );
				frame_register_tokens( frame->last, frame->expr );
				frame_free( frame );

				DEBUGP( "[%s] Doing tail call elimination\n", __func__ );

			} else {
				temp_frame = expand_procedure( frame, frame->expr );
			}

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
				frame_register_one_token( frame, foo );

				foo->next = frame->expr->next;
				frame->expr = foo;
				apply = false;
			}

			break;

		case TYPE_HASHMAP:
			{
				foo = ext_proc_token( builtin_hashmap_get );
				frame_register_one_token( frame, foo );

				foo->next = frame->expr;
				frame->expr = foo;
				frame->ntokens++;
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

		gc_mark_tree( frame->value );
		frame->heap = gc_sweep( frame->heap );
		frame->value->next = NULL;

		frame_add_token_noclone( temp_frame, frame->value );

		frame_free( frame );
		*frame_ret = temp_frame;
	}

	return ret;
}

