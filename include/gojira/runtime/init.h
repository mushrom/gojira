#ifndef _GOJIRA_RUNTIME_INIT_H
#define _GOJIRA_RUNTIME_INIT_H 1
#include <gojira/runtime/runtime.h>

runtime_t *initialize_runtime( unsigned flags );
void deinit_runtime( runtime_t *runtime );

#endif
