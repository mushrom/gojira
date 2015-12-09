#include <gojira/api/api.h>

gojira_t *goj_default_runtime( ){
	gojira_t *ret;
	
	ret = frame_create( NULL, NULL, MAKE_ENV );
	init_global_frame( ret );
	evaluate_file( ret, BASE_LIB );

	return ret;
}

goj_val_t *goj_parse( char *str ){
	goj_val_t *ret;

	ret = parse_scheme_tokens( str );
	gc_unmark( ret );

	return ret;
}

goj_val_t *goj_eval( gojira_t *runtime, goj_val_t *tree ){
	runtime->ptr = tree;
	eval_loop( runtime );

	return runtime->end;
}

goj_val_t *goj_eval_string( gojira_t *runtime, char *str ){
	goj_val_t *ret = NULL;
	goj_val_t *values;
	
	values = goj_parse( str );
	ret = goj_eval( runtime, values );
	goj_free_val( values );

	return ret;
}

void goj_print( goj_val_t *value ){
	print_token( value, OUTPUT_REGULAR );
}

void goj_println( goj_val_t *value ){
	print_token( value, OUTPUT_REGULAR );
	putchar( '\n' );
}

void goj_free_val( goj_val_t *value ){
	free_tokens( value );
}

void goj_free_runtime( gojira_t *runtime ){
	gc_sweep( runtime->heap );
	frame_free( runtime );
}
