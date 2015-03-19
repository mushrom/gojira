#ifndef _GOJIRA_RUNTIME_SYNTAX_H
#define _GOJIRA_RUNTIME_SYNTAX_H

#include <gojira/tokens.h>
#include <gojira/runtime/runtime.h>

token_t *expand_lambda( stack_frame_t *frame, token_t *tokens );
stack_frame_t *expand_procedure( stack_frame_t *frame, token_t *tokens );
token_t *expand_if_expr( stack_frame_t *frame, token_t *tokens );
token_t *expand_syntax_rules( stack_frame_t *frame, token_t *tokens );
token_t *expand_vector( stack_frame_t *frame, token_t *tokens );

#endif
