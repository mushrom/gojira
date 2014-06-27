#include <gojira/config.h>
#include <gojira/libs/hashmap.h>
#include <string.h>
#include <stdlib.h>

static hashmap_t *config_map;

void initialize_config( ){
	config_map = hashmap_create( 32 );
}

unsigned hash_string( char *str ){
	unsigned ret = 0, i;

	for ( i = 0; str[i]; i++ ){
		ret ^= str[i] * (i + 1);
		ret *= str[i];
	}

	return ret;
}

void set_config_option( char *name, long int value ){
	config_opt_t *new_option;

	new_option = hashmap_get( config_map, hash_string( name ));

	if ( new_option ){
		new_option->value = value;

	} else {
		new_option = calloc( 1, sizeof( config_opt_t ));
		new_option->name = strdup( name );
		new_option->value = value;

		hashmap_add( config_map, hash_string( name ), new_option );
	}
}

long int get_config_option( char *name ){
	config_opt_t *opt;
	long int ret = 0;

	opt = hashmap_get( config_map, hash_string( name ));

	if ( opt )
		ret = opt->value;

	return ret;
}
