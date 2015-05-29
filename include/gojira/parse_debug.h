#ifndef _GOJIRA_DEBUG_PARSER_H
#define _GOJIRA_DEBUG_PARSER_H
#ifdef __cplusplus
extern "C" {
#endif

#include <gojira/tokens.h>
#include <gojira/config.h>

#if GOJIRA_DEBUG
#define DEBUGP(format, ...) fprintf( stderr, format, __VA_ARGS__ );
#else
#define DEBUGP(format, ...) /* debug statement, format __VA_ARGS__ */;
#endif

char *type_str( type_t type );

#ifdef __cplusplus
}
#endif
#endif
