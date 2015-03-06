#ifndef _GOJIRA_RUNTIME_FILES_H
#define _GOJIRA_RUNTIME_FILES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdbool.h>

char *read_input_file( FILE *fp );
bool evaluate_file( stack_frame_t *frame, char *filename );

#ifdef __cplusplus
}
#endif
#endif
