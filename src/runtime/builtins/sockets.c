#include <gojira/config.h>

#if GOJIRA_ENABLE_SOCKETS
#include <gojira/runtime/runtime.h>
#include <gojira/runtime/builtin.h>
#include <gojira/parse_debug.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

/* networking stuff */
#include <sys/socket.h>
#include <sys/types.h>
#include <netdb.h>

token_t *builtin_tcp_socket( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;
	int sock;

	if ( frame->ntokens == 3 ){
		move = frame->expr->next;

		if ( move->type == TYPE_STRING ){
			if ( move->next->type == TYPE_NUMBER ){
				struct hostent *host;
				struct sockaddr_in server_addr;
				char *hostname = shared_get( move->data );

				host = gethostbyname( hostname );

				if ( host && ( sock = socket( AF_INET, SOCK_STREAM, 0 )) >= 0 ){
					server_addr.sin_family = AF_INET;
					server_addr.sin_port = htons( move->next->smalldata );
					server_addr.sin_addr = *((struct in_addr *)host->h_addr);

					if ( connect( sock, (struct sockaddr *)&server_addr,
					              sizeof( struct sockaddr)) >= 0 )
					{
						ret = alloc_token( );
						ret->type = TYPE_SOCKET;
						ret->smalldata = (unsigned)sock;

					} else {
						frame->error_call( frame, "[%s] Could not connect to host\n",
							__func__, frame->ntokens - 1 );
					}

				} else {
					frame->error_call( frame, "[%s] Could not open socket\n",
						__func__, frame->ntokens - 1 );
				}

			} else {
				frame->error_call( frame, "[%s] Expected argument 2 to be a number, but have %s\n",
					__func__, type_str( move->next->type ));
			}

		} else {
			frame->error_call( frame, "[%s] Expected argument 1 to be a string, but have %s\n",
				__func__, type_str( move->type ));
		}
		
	} else {
		frame->error_call( frame, "[%s] Expected 2 arguments, but have %d\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_tcp_getchar( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;

	if ( frame->ntokens == 2 ){
		move = frame->expr->next;

		if ( move->type == TYPE_SOCKET ){
			int recv_ret;
			unsigned char ch;

			recv_ret = recv( move->smalldata, &ch, 1, 0 );
			
			if ( recv_ret > 0 ){
				ret = alloc_token( );
				ret->type = TYPE_CHAR;
				ret->smalldata = ch;

			} else {
				ret = alloc_token( );
				ret->type = TYPE_BOOLEAN;
				ret->smalldata = false;
			}

		} else {
			frame->error_call( frame, "[%s] Expected argument 1 to be a socket, but have %s\n",
				__func__, type_str( move->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Expected 1 argument, but have %d\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}

token_t *builtin_tcp_putchar( stack_frame_t *frame ){
	token_t *ret = NULL;
	token_t *move;

	if ( frame->ntokens == 3 ){
		move = frame->expr->next;

		if ( move->type == TYPE_SOCKET ){
			if ( move->next->type == TYPE_CHAR ){
				int send_ret;
				unsigned char ch = move->next->smalldata;

				send_ret = send( move->smalldata, &ch, 1, 0 );
				
				if ( send_ret > 0 ){
					ret = alloc_token( );
					ret->type = TYPE_CHAR;
					ret->smalldata = ch;
				}
			}

		} else {
			frame->error_call( frame, "[%s] Expected argument 1 to be a socket, but have %s\n",
				__func__, type_str( move->type ));
		}

	} else {
		frame->error_call( frame, "[%s] Expected 1 argument, but have %d\n",
			__func__, frame->ntokens - 1 );
	}

	return ret;
}
#endif
