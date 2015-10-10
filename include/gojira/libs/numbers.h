#ifndef _GOJIRA_NUMBERS_H
#define _GOJIRA_NUMBERS_H 1

#include <stdio.h>

typedef struct number_buf {
	type_t type;

	union {
		long int          s_int;
		unsigned long int u_int;
		double            real;
		// TODO: implement a big int type
	};
} number_t;

static inline type_t determine_type( number_t a, number_t b ){
	return (a.type == TYPE_REAL || b.type == TYPE_REAL)? TYPE_REAL : TYPE_NUMBER;
}

#define NUMBER_DATA(x) (((x).type == TYPE_REAL)? (x).real : (x).s_int)

#define NUMBER_OP(NAME, OP) \
	static inline number_t number_##NAME( number_t a, number_t b ){ \
		number_t ret; \
		\
		ret.type = determine_type( a, b ); \
		switch( ret.type ){ \
			case TYPE_REAL: \
				ret.real = NUMBER_DATA(a) OP NUMBER_DATA(b); \
				break; \
			case TYPE_NUMBER: \
				ret.s_int = NUMBER_DATA(a) OP NUMBER_DATA(b); \
				break; \
			default: \
				puts( "Error: Have incompatible number type in number_" #NAME ); \
				break; \
		} \
		\
		return ret; \
	}

NUMBER_OP( add, + )
NUMBER_OP( sub, - )
NUMBER_OP( mul, * )
NUMBER_OP( div, / )

static inline number_t as_int_number( int val ){
	return (number_t){
		.type = TYPE_NUMBER,
		.s_int = val,
	};
}

static inline number_t as_real_number( double val ){
	return (number_t){
		.type = TYPE_REAL,
		.real = val,
	};
}

#endif
