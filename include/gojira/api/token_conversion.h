#ifndef _GOJIRA_API_TOKEN_CONVERSION_H
#define _GOJIRA_API_TOKEN_CONVERSION_H 1

#include <gojira/api/api.h>
#include <gojira/libs/shared.h>
#include <string.h>

#define GOJ_INT(N) \
	&(goj_val_t){ \
		.type = TYPE_NUMBER, \
		.number = as_int_number( N ), \
	}

#define GOJ_REAL(N) \
	&(goj_val_t){ \
		.type = TYPE_REAL, \
		.number = as_real_number( N ), \
	}

#define GOJ_SYM(N) \
	&(goj_val_t){ \
		.type = TYPE_SYMBOL, \
		.flags = T_FLAG_HAS_SHARED, \
		.data = shared_new( strdup( N ), DEFAULT_DTOR ), \
	}

#endif
