#ifndef _GOJIRA_RUNTIME_PRINTER_H
#define _GOJIRA_RUNTIME_PRINTER_H 1
#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>

typedef enum {
	OUTPUT_REGULAR,
	OUTPUT_READABLE,
} print_readable_t;

token_t *file_dump_tokens( FILE *fp, token_t *tokens, print_readable_t readable );
void file_print_token( FILE *fp, token_t *token, print_readable_t readable );
token_t *dump_tokens( token_t *tokens );
void print_token( token_t *token, print_readable_t readable );

#ifdef __cplusplus
}
#endif
#endif
