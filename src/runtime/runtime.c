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

token_t *eval_loop( stack_frame_t *base, token_t *tokens ){
	stack_frame_t *frame = base;
	token_t *ret = tokens;

	bool running = true;
	bool have_error = false;

	while ( running && !have_error ){
		// Evaluate sub-expressions
		if ( frame->ptr ){
			have_error = eval_frame_subexpr( &frame, base );

		// Evaluation finished, apply function
		} else {
			if ( frame->last ){
				have_error = eval_frame_expr( &frame, base );

			} else {
				running = false;
			}
		}
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
				printf( "[%s] Error: Empty expression\n", __func__ );

				stack_trace( frame );
				ret = true;
				break;
			}

			frame = *frame_ret = frame_create( frame, move->down );
			break;

		case TYPE_SYMBOL:
			if ( frame->ptr->down ){
				frame_add_token( frame, frame->ptr->down );
				frame->ptr = frame->ptr->next;

			} else {
				// search for symbol in the highest scope, which has the
				// most often used variables
				move = frame_find_var( first, frame->ptr->data );

				// didn't find it, start from top frame
				if ( !move )
					move = frame_find_var( frame, frame->ptr->data );

				if ( move ){
					frame_add_token( frame, move );

					if ( move->type != TYPE_SYNTAX ){
						frame->ptr = frame->ptr->next;

					} else {
						for ( frame->ptr = frame->ptr->next; frame->ptr; frame->ptr = frame->ptr->next )
							frame_add_token( frame, frame->ptr );
					}

				} else {
					printf( "[%s] Error: undefined variable \"%s\"\n",
							__func__, (char *)frame->ptr->data );

					stack_trace( frame );
					ret = true;
				}
			}

			break;

		case TYPE_LAMBDA:
			for ( ; frame->ptr; frame->ptr = frame->ptr->next )
				frame_add_token( frame, frame->ptr );

			break;

		case TYPE_DEF_SYNTAX:
			frame_add_token( frame, frame->ptr );
			frame_add_token( frame, frame->ptr->next );
			frame->ptr = frame->ptr->next->next;
			break;

		case TYPE_SYNTAX_RULES:
			move = calloc( 1, sizeof( token_t ));
			move->type = TYPE_SYNTAX;
			move->down = frame->ptr;

			frame_add_token_noclone( frame, ext_proc_token( builtin_return_first ));
			frame_add_token( frame, move );

			frame->ptr = NULL;
			break;

		case TYPE_IF:
			move = expand_if_expr( frame, frame->ptr );

			if ( move ){
				frame_add_token_noclone( frame, move );
				frame->ptr = move->next;

			} else {
				ret = true;
			}

			break;

		case TYPE_QUOTED_TOKEN:
			frame_add_token( frame, frame->ptr->down );
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
	ext_proc_t *ext;
	scheme_func handle;

	frame->status = frame->expr->type;

	switch ( frame->expr->type ){
		case TYPE_EXTERN_PROC:
			handle = frame->expr->data;

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
			foo = frame_alloc_token( frame );
			foo->type = TYPE_PROCEDURE;
			foo->down = frame->expr;
			frame->value = foo;
			break;

		case TYPE_IF:
			if ( frame->expr->next->type == TYPE_BOOLEAN
					&& frame->expr->next->smalldata == false ){
				foo = clone_token_tree( frame->expr->down->next );

			} else {
				foo = clone_token_tree( frame->expr->down );
			}

			gc_unmark( foo );
			frame_register_token( frame, foo );
			frame->expr = frame->end = NULL;

			if ( foo->type == TYPE_LIST ){
				// If it's an expression, evaluate the tokens in the current frame.
				// TODO: find a less hackish way to do this
				frame->ptr = foo->down;

			} else {
				frame_add_token_noclone( frame, ext_proc_token( builtin_return_first ));
				frame->ptr = foo;
			}

			apply = false;
			break;

		case TYPE_DEF_SYNTAX:
			frame_add_var( frame->last, frame->expr->next->data, frame->expr->next->next );

			frame->value = frame_alloc_token( frame );
			frame->value->type = TYPE_NULL;

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

		default:

			printf( "[%s] Can't apply \"%s\"\n", __func__, type_str( frame->expr->type ));
			stack_trace( frame );
			ret = true;
			break;
	}

	if ( apply && !ret ){
		temp_frame = frame->last;

		//gc_dump( frame );
		gc_mark( frame->value );
		frame->heap = gc_sweep( frame->heap );

		frame_add_token_noclone( temp_frame, frame->value );

		frame_free( frame );
		*frame_ret = temp_frame;
	}

	return ret;
}

