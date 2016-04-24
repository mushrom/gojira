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

token_t *builtin_get_last_continuation( stack_frame_t *frame ){
	token_t *ret = NULL;

	if ( frame->last && frame->last->last ){
		stack_frame_t *temp = frame->last->last;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_CONTINUATION;

		if ( temp->last && temp->status == TYPE_PROCEDURE ){
			temp = temp->last;
		}

		ret->cont = frame_capture( temp );
	}

	return ret;
}

token_t *builtin_car( stack_frame_t *frame ){
	token_t *move = frame->expr->next;
	token_t *ret = NULL;

	if ( move && move->type == TYPE_LIST && move->down ){
		ret = gc_clone_token( get_current_gc( frame ), move->down );
		ret->next = NULL;

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

static bool set_variable( stack_frame_t *frame, const token_t *tokens, unsigned mutable ){
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
			free_tokens( var->token );
			var->token = clone_token_tree( tokens->next );

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
	unsigned mutable = VAR_MUTABLE;
	bool error = false;

	if ( frame->ntokens == 3 ){
		move = frame->expr->next;

	} else if ( frame->ntokens == 4 ){
		token_t *mut_spec = frame->expr->next;
		move = frame->expr->next->next;

		if ( mut_spec->type == TYPE_SYMBOL ){
			char *str = shared_get( mut_spec->data );

			if ( strcmp( str, ":mut" ) == 0 ){
				mutable = VAR_MUTABLE;

			} else if ( strcmp( str, ":immut" ) == 0 ){
				mutable = VAR_IMMUTABLE;

			} else if ( strcmp( str, ":builtin" ) == 0 ){
				mutable = VAR_MUTABLE_BUILTIN;

			} else {
				FRAME_ERROR( frame,
					"invalid mutability specifier: %s",
					str );

				error = true;
			}

		} else {
			FRAME_ERROR_ARGTYPE( frame, symbol, mut_spec->type );
			error = true;
		}

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
		ret->boolean = tok->type == TYPE_SYMBOL && tok->down == NULL;

	} else {
		FRAME_ERROR_ARGNUM( frame, 1 );
	}

	return ret;
}

token_t *builtin_lessthan( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *op1, *op2;
	bool val = false;

	if ( frame->ntokens == 3 ){
		op1 = frame->expr->next;
		op2 = frame->expr->next->next;

		ret = gc_alloc_token( get_current_gc( frame ));
		ret->type = TYPE_BOOLEAN;

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

	if ( frame->ntokens == 1 ){
		stack_trace( frame );

	} else if ( frame->ntokens == 2 &&
	            frame->expr->next->type == TYPE_CONTINUATION )
	{
		stack_trace( frame->expr->next->cont );

	} else {
		FRAME_ERROR( frame,
			"Expected no arguments or a continuation, but have %u args\n",
			frame->ntokens );

	}

	ret = gc_alloc_token( get_current_gc( frame ));
	ret->type = TYPE_NULL;

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
	bool eval_return;

	if ( frame->ntokens == 2 ){
		if ( frame->expr->next->type == TYPE_STRING ){
			char *fname = shared_get( frame->expr->next->data );

			env = frame->env;
			for ( ; env->last; env = env->last );

			global = frame;
			for ( ; global->last; global = global->last );

			tempframe = frame_create( frame, NULL, DONT_MAKE_ENV );
			tempframe->flags |= RUNTIME_FLAG_NO_EVAL;
			tempframe->env = env;
			eval_return = evaluate_file( tempframe, fname );

			ret = gc_alloc_token( get_current_gc( frame ));
			ret->type = TYPE_BOOLEAN;
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
