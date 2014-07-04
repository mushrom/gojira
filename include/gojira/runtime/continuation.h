#ifndef _GOJIRA_RUNTIME_CONTINUATION_H
#define _GOJIRA_RUNTIME_CONTINUATION_H 1
#include <gojira/tokens.h>

typedef struct continuation {
	token_t *pointer;
	token_t *value;
} continuation_t;

#endif
