#ifndef _GOJIRA_PORTS_H
#define _GOJIRA_PORTS_H 1
#include <gojira/tokens.h>
#include <stdbool.h>

typedef enum {
	GOJ_PORT_NONE    = 0,
	GOJ_PORT_CLOSED  = 1,
	GOJ_PORT_OPEN    = 2,
	GOJ_PORT_INPUT   = 4,
	GOJ_PORT_OUTPUT  = 8,
	GOJ_PORT_ASYNC   = 16,
} goj_port_status_t;

typedef struct goj_port goj_port_t;

typedef int (*goj_port_write)( goj_port_t *port, token_t *obj, bool readable );
typedef int (*goj_port_read)( goj_port_t *port );
typedef int (*goj_port_read_char)( goj_port_t *port );
typedef int (*goj_port_close)( goj_port_t *port, unsigned mask );

typedef struct goj_port {
	goj_port_status_t status;

	goj_port_write     write;
	goj_port_read      read;
	goj_port_read_char read_char;
	goj_port_close     close;

	void *portdata;
} goj_port_t;

token_t *builtin_close_port( stack_frame_t *frame );
token_t *builtin_close_input_port( stack_frame_t *frame );
token_t *builtin_close_output_port( stack_frame_t *frame );

#endif
