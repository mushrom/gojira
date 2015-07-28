#ifndef _GOJIRA_LEXER_H
#define _GOJIRA_LEXER_H 1
#include <gojira/tokens.h>
#ifdef __cplusplus
extern "C" {
#endif

token_t *lexerize( const char *string );

#ifdef __cplusplus
}
#endif
#endif
