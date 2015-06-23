#ifndef _GOJIRA_DEBUGGER_H
#define _GOJIRA_DEBUGGER_H 1
#include <gojira/runtime/frame.h>
#include <gojira/tokens.h>

token_t *debugger_loop( stack_frame_t *frame );

#endif
