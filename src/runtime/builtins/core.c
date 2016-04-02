#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/builtin.h>
#include <gojira/runtime/files.h>
#include <gojira/libs/numbers.h>
#include <gojira/parse_debug.h>

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

token_t *ext_proc_token( scheme_func handle ){
	token_t *ret = NULL;

	ret = alloc_token( );
	ret->func = handle;
	ret->type = TYPE_EXTERN_PROC;

	return ret;
}

token_t *builtin_car( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = NULL;

	if ( move && move->type == TYPE_LIST && move->down ){
		//ret = move->down;
		ret = gc_clone_token( get_current_gc( frame ), move->down );
		ret->next = NULL;

		/*
    } else if ( move && move->type == TYPE_ITERATOR ){
        ret = builtin_iterator_access( frame );
		*/

	} else {
		FRAME_ERROR_ARGTYPE( frame, "list", move->type );
	}

	return ret;
}

token_t *builtin_cdr( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = NULL;

	if ( move && move->type == TYPE_LIST && move->down ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_LIST;
		ret->down = move->down->next;

		/*
    } else if ( move && move->type == TYPE_ITERATOR ){
        ret = builtin_iterator_next( frame );
		*/

	} else {
		FRAME_ERROR_ARGTYPE( frame, "list", move->type );
	}

	return ret;
}

token_t *builtin_cons( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *temp;
	token_t *ret = NULL;

	if ( move && move->next ){
		if ( move->next->type == TYPE_LIST ){
			//temp = clone_token_tree( move );
			temp = gc_clone_token( get_current_gc( frame ), move );
			temp->next = move->next->down;

			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_LIST;
			ret->down = temp;
			ret->next = NULL;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "list", move->next->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_equal( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;
	bool val = false;

	if ( frame->ntokens == 3 ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;

		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		if ( op1->type == op2->type ){
			switch( op1->type ){
				case TYPE_STRING:
				case TYPE_SYMBOL:
					val = strcmp( shared_get( op1->data ),
					              shared_get( op2->data )) == 0;
					break;

				case TYPE_CHAR:
					val = op1->character == op2->character;
					break;

				case TYPE_REAL:
					val = op1->number.real == op2->number.real;
					break;

				case TYPE_NUMBER:
					val = op1->number.s_int == op2->number.s_int;
					break;

				default:
					val = ( op1->data == op2->data )
					   && ( op1->down == op2->down );
					break;
			}

		} // default value of 'val' is false, so just continue

		ret->boolean = val;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_greaterthan( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;
	bool val = false;

	if ( frame->ntokens == 3 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		/*
		ret->smalldata = ( op1->type          == op2->type )
		              && ((int)op1->smalldata > (int)op2->smalldata );

	     */

		if ( op1->type == op2->type ){
			switch( op1->type ){
				case TYPE_CHAR:
					val = op1->character > op2->character;
					break;

				case TYPE_REAL:
					val = op1->number.real > op2->number.real;
					break;

				case TYPE_NUMBER:
					val = op1->number.s_int > op2->number.s_int;
					break;

				default:
					break;
			}
		}

		ret->boolean = val;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

static bool set_variable( stack_frame_t *frame, const token_t *tokens, bool mutable ){
	token_t *temp;
	variable_t *var;
	bool ret = false;

	if ( tokens->type == TYPE_SYMBOL ){
		char *varname = shared_get( tokens->data );
		temp = gc_clone_token( get_current_gc( frame ), tokens->next );
		if ( env_add_var( frame->env, varname, temp, NO_RECURSE, mutable )){
			ret = true;
		}

	} else if ( tokens->type == TYPE_VARIABLE_REF ){
		var = shared_get( tokens->data );
		temp = var->token;

		if ( var->is_mutable ){
			//var->token = gc_clone_token( get_current_gc( frame ), tokens->next );
			free_tokens( var->token );
			var->token = clone_token_tree( tokens->next );
			//var->token = clone_token( tokens->next );
			//frame_register_tokens( frame, temp );
			//gc_mark_tree( tokens->next );
			ret = true;

		} else {
			FRAME_ERROR( frame,
				"variable \"%s\" is not mutable",
				var->key );
		}

	} else {
		FRAME_ERROR( frame,
			"expected symbol or variable reference, but have %s",
			type_str( tokens->type ));
	}

	return ret;
}

token_t *builtin_intern_set( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	bool mutable = false;
	bool error = false;

	if ( frame->ntokens == 3 ){
		move = frame->expr->next;

	} else if ( frame->ntokens == 4 ){
		move = frame->expr->next->next;
		mutable = true;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
		error = true;
	}

	if ( !error ){
		if ( !set_variable( frame, move, mutable )){
			error = true;

		} else {
			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NULL;
		}
	}

	return ret;
}

token_t *builtin_intern_set_global( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	env_t *first;

	move = frame->expr->next;

	if ( frame->ntokens == 3 ){
		if ( move->type == TYPE_SYMBOL ){
			char *varname = shared_get( move->data );

			token_t *temp = gc_clone_token( get_current_gc( frame ), move->next );
			env_add_var( frame->env, varname, temp, RECURSE, VAR_MUTABLE );

			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_NULL;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "symbol", move->type );
		}

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_is_list( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;

	if ( frame->ntokens == 2 ){
		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;

		move = frame->expr->next;
		//ret->smalldata = move->type == TYPE_LIST;
		ret->boolean = move->type == TYPE_LIST;

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

token_t *builtin_is_null( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *tok;

	if ( frame->ntokens == 2 ){
		tok = frame->expr->next;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		//ret->smalldata = tok->type == TYPE_LIST && tok->down == NULL;
		ret->boolean = tok->type == TYPE_LIST && tok->down == NULL;

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

token_t *builtin_is_symbol( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *tok;

	if ( frame->ntokens == 2 ){
		tok = frame->expr->next;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		//ret->smalldata = tok->type == TYPE_SYMBOL && tok->down == NULL;
		ret->boolean = tok->type == TYPE_SYMBOL && tok->down == NULL;

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

/*
token_t *builtin_lessthan( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;

	if ( frame->ntokens == 3 ){
		ret = alloc_token( );
		ret->type = TYPE_BOOLEAN;

		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		ret->smalldata = ( op1->type          == op2->type )
			          && ((int)op1->smalldata < (int)op2->smalldata );

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}
*/

token_t *builtin_lessthan( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;
	bool val = false;

	if ( frame->ntokens == 3 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;
		/*
		ret->smalldata = ( op1->type          == op2->type )
		              && ((int)op1->smalldata > (int)op2->smalldata );

	     */

		if ( op1->type == op2->type ){
			switch( op1->type ){
				case TYPE_CHAR:
					val = op1->character < op2->character;
					break;

				case TYPE_REAL:
					val = op1->number.real < op2->number.real;
					break;

				case TYPE_NUMBER:
					val = op1->number.s_int < op2->number.s_int;
					break;

				default:
					break;
			}
		}

		ret->boolean = val;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_list( stack_frame_t *frame ){
	token_t *ret = NULL;

	ret = gc_alloc_token( get_current_gc( frame ));
	ret->type = TYPE_LIST;
	ret->down = frame->expr->next;

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

	ret = gc_alloc_token( get_current_gc( frame ));
	ret->type = TYPE_NULL;
	stack_trace( frame );

	return ret;
}

token_t *builtin_sleep( stack_frame_t *frame ){
	token_t *ret;

	ret = gc_alloc_token( get_current_gc( frame ));
	ret->type = TYPE_NULL;
	sleep( 1 );

	return ret;
}

token_t *builtin_true( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		ret = frame->expr->next;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_false( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->ntokens == 3 ){
		ret = frame->expr->next->next;

	} else {
		FRAME_ERROR_ARGNUM( frame, 2 );
	}

	return ret;
}

token_t *builtin_load_global_file( stack_frame_t *frame ){
	token_t *ret = NULL;
	stack_frame_t *tempframe;
	stack_frame_t *global;
	env_t *env;
	token_t *oldptr;
	bool eval_return;

	if ( frame->ntokens == 2 ){
		if ( frame->expr->next->type == TYPE_STRING ){
			char *fname = shared_get( frame->expr->next->data );
			//tempframe = frame;
			//for ( ; tempframe->last; tempframe = tempframe->last );

			//oldptr = tempframe->ptr;
			//eval_return = evaluate_file( tempframe, fname );
			//tempframe->ptr = oldptr;
			env = frame->env;
			for ( ; env->last; env = env->last );

			global = frame;
			for ( ; global->last; global = global->last );

			tempframe = frame_create( frame, NULL, DONT_MAKE_ENV );
			tempframe->flags |= RUNTIME_FLAG_NO_EVAL;
			//tempframe->garbage = calloc( 1, sizeof( gbg_collector_t ));
			//gc_init( frame->garbage, tempframe->garbage );
			//tempframe->gc.id = frame->gc.id + 1;
			tempframe->env = env;
			eval_return = evaluate_file( tempframe, fname );
			//gc_collect( &tempframe->gc, NULL, 0 );
			//gc_merge( get_current_gc( frame ), &tempframe->gc );
			//gc_merge( &global->gc, &tempframe->gc );
			//gc_collect( get_current_gc( tempframe ));
			//gc_merge( get_current_gc( global ), get_current_gc( tempframe ));
			frame_free( tempframe );

			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type      = TYPE_BOOLEAN;
			//ret->smalldata = eval_return;
			ret->boolean = eval_return;

		} else {
			FRAME_ERROR_ARGTYPE( frame, "string", frame->expr->next->type );
		}

	} else {
		// not enough tokens
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}
