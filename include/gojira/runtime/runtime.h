#ifndef _GOJIRA_RUNTIME_RUNTIME_H
#define _GOJIRA_RUNTIME_RUNTIME_H 1

#include <gojira/tokens.h>
#include <gojira/runtime/frame.h>
#include <gojira/libs/list.h>
#include <gojira/libs/hashmap.h>
#include <stdbool.h>

token_t *eval_loop( stack_frame_t *base, token_t *tokens );
bool eval_frame_subexpr( stack_frame_t **frame_ret, stack_frame_t *first );
bool eval_frame_expr( stack_frame_t **frame_ret, stack_frame_t *first );

#endif
