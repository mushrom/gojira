#include <gojira/config.h>
#include <gojira/debugger/debugger.h>
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/garbage.h>
#include <stdio.h>
#include <string.h>

static unsigned p_heap( token_t *heap ){
	token_t *move = heap;
	unsigned i = 0;
	unsigned unmarked = 0;
	unsigned marked = 0;
	unsigned freed = 0;

	for ( ; move; move = move->gc_link ){
		switch ( move->status ){
			case GC_UNMARKED: unmarked++; break;
			case GC_MARKED:   marked++; break;
			case GC_FREED:    freed++; break;
			default: break;
		};

		i++;
	}

	printf( "%3u tokens, %3u unmarked, %3u marked, %3u freed",
			i, unmarked, marked, freed );

	return i;
}

static unsigned dump_frame_heaps( stack_frame_t *frame, unsigned limit, unsigned n ){
	unsigned total = 0;

	if ((( limit && n < limit ) || !limit ) && frame ){
		printf( "frame %3u: ", n );
		total += p_heap( frame->heap );
		printf( "\n" );
		total += dump_frame_heaps( frame->last, limit, n + 1 );
	}

	return total;
}

token_t *debugger_loop( stack_frame_t *frame ){
	char *buf = malloc( 128 );
	bool running = true;
	token_t *ret = alloc_token( );
	ret->type = TYPE_NULL;

	while ( running ){
		printf( "debug > " );
		fgets( buf, 128, stdin );

		if ( buf ){
			if ( strcmp( buf, "exit\n" ) == 0 || strcmp( buf, "cont" ) == 0 ){
				printf( "[%s] All done\n", __func__ );
				running = false;
				break;

			} else if ( strcmp( buf, "status\n" ) == 0 ) {
				printf( "[%s] Status info coming soon\n", __func__ );

			} else if ( strcmp( buf, "help\n" ) == 0 ){
				printf( "%8s | show this help\n", "help" );
				printf( "%8s | show a trace of each frame's heap\n", "heap" );
				printf( "%8s | show the interpreter's current status\n", "status" );

			} else if ( strcmp( buf, "heap\n" ) == 0 ){
				printf( "%3u tokens total.\n", dump_frame_heaps( frame, 10000, 0 ));

			} else {
				printf( "[%s] Unknown command.\n", __func__ );
			}

		} else {
			break;
		}
	}

	return ret;
}
