#ifndef _GOJIRA_CONFIG_H
#define _GOJIRA_CONFIG_H 1

/* Compile-time configuration options */
#ifndef _GOJIRA_DEBUG
#define _GOJIRA_DEBUG 1
#endif /* _GOJIRA_DEBUG */

typedef struct config_opt {
	char *name;
	long int value;
} config_opt_t;

void initialize_config( );
void set_config_option( char *name, long int value );
long int get_config_option( char *name );

#endif
