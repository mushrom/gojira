#include <gojira/config.h>
#include <gojira/debugger/debugger.h>
#include <linenoise/linenoise.h>
#include <stdio.h>
#include <string.h>

token_t *debugger_loop( stack_frame_t *frame ){
	char *buf;
	bool running = true;
	token_t *ret = alloc_token( );
	ret->type = TYPE_NULL;

	while ( running ){
		buf = linenoise( "debug > " );

		if ( buf ){
			if ( strcmp( buf, "exit" ) == 0 ){
				printf( "[%s] All done\n", __func__ );
				running = false;
				break;

			} else if ( strcmp( buf, "status" ) == 0 ) {
				printf( "[%s] Status info coming soon\n", __func__ );

			} else {
				printf( "[%s] Unknown command.", __func__ );
			}
		}
	}

	return ret;
}
