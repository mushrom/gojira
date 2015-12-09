#ifndef _GOJIRA_API_H
#define _GOJIRA_API_H
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/files.h>
#include <gojira/runtime/frame.h>
#include <gojira/runtime/garbage.h>
#include <gojira/runtime/printer.h>
#include <gojira/config.h>

typedef stack_frame_t gojira_t;
typedef token_t goj_val_t;

gojira_t *goj_default_runtime( );
gojira_t *goj_runtime( error_printer handler );

goj_val_t *goj_parse( char *str );
goj_val_t *goj_eval( gojira_t *runtime, goj_val_t *tree );
goj_val_t *goj_eval_string( gojira_t *runtime, char *str );

void goj_print( goj_val_t *value );
void goj_println( goj_val_t *value );

void goj_free_val( goj_val_t *value );
void goj_free_runtime( gojira_t *runtime );

#endif
