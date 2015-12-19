#ifndef _GOJIRA_API_H
#define _GOJIRA_API_H
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/files.h>
#include <gojira/runtime/frame.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/printer.h>
#include <gojira/config.h>

#include <stdarg.h>

typedef stack_frame_t gojira_t;
typedef token_t goj_val_t;

typedef struct goj_call {
	gojira_t *frame;
	goj_val_t *ptr;
	goj_val_t *ptr_end;
} goj_call_t;

gojira_t *goj_default_runtime( );
gojira_t *goj_runtime( error_printer handler );

goj_call_t *goj_start_call( gojira_t *call );
goj_call_t *goj_call_add( goj_call_t *call, goj_val_t *val );
goj_val_t *goj_exec_call( goj_call_t *call );
goj_call_t *goj_build_call( gojira_t *env, goj_val_t *first, ... );

goj_val_t *goj_parse( char *str );
goj_val_t *goj_eval( gojira_t *runtime, goj_val_t *tree );
goj_val_t *goj_eval_string( gojira_t *runtime, char *str );
goj_val_t *goj_eval_file( gojira_t *runtime, char *fname );

void goj_print( goj_val_t *value );
void goj_println( goj_val_t *value );

void goj_free_val( goj_val_t *value );
void goj_free_runtime( gojira_t *runtime );
void goj_free_call( goj_call_t *call );

#include <gojira/api/token_conversion.h>

#endif
