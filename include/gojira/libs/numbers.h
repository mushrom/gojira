#ifndef _GOJIRA_NUMBERS_H
#define _GOJIRA_NUMBERS_H 1

#include <stdio.h>

typedef struct number_buf {
	type_t type;

	union {
		long int          s_int;
		unsigned long int u_int;
		double            real;

		struct {
			long int num;
			unsigned long denom;
		};
		// TODO: implement a big int type
	};
} number_t;

static inline type_t determine_type( number_t a, number_t b ){
	return (a.type > b.type)? a.type : b.type;
}

#define NUMBER_DATA(x) \
	(((x).type == TYPE_REAL)?     (x).real : \
	 ((x).type == TYPE_RATIONAL)? ((double)(x).num / (x).denom) : \
	  (x).s_int)

/* XXX: this gets rid of the body of 'case TYPE_NUMBER:' in the switch statement of
 *      NUMBER_OP when expanding the division operation, to allow it to fall through
 *      to the rational operation. Kind of gross, sorry future me.
 */
#define NUMBER_HANDLE_div(...) /* __VA_ARGS__ */
#define NUMBER_HANDLE_add(...) __VA_ARGS__
#define NUMBER_HANDLE_sub(...) __VA_ARGS__
#define NUMBER_HANDLE_mul(...) __VA_ARGS__

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
				NUMBER_HANDLE_##NAME( \
					ret.s_int = NUMBER_DATA(a) OP NUMBER_DATA(b); \
					break; \
			    ) \
			case TYPE_RATIONAL: \
				ret = rational_##NAME( as_rational_number(a), as_rational_number(b)); \
				break; \
			default: \
				puts( "Error: Have incompatible number type in number_" #NAME ); \
				break; \
		} \
		\
		return ret; \
	}

static inline number_t normalize_rational( number_t foo );

#define RATIONAL_OP(NAME, NUM, DENOM) \
	static inline number_t rational_##NAME( number_t a, number_t b ){ \
		number_t ret = { \
			.type = TYPE_RATIONAL, \
			.num = (NUM), \
			.denom = (DENOM), \
		}; \
		return normalize_rational( ret ); \
	}

// a/b + c/d == (ad + bc) / bd
RATIONAL_OP( add,
	((a.num * b.denom) + (b.num * a.denom)),
	(a.denom * b.denom))

// a/b - c/d == (ad - bc) / bd
RATIONAL_OP( sub,
	((a.num * b.denom) - (b.num * a.denom)),
	(a.denom * b.denom))

// a/b * c/d == ac / bd
RATIONAL_OP( mul, (a.num * b.num), (a.denom * b.denom))

// a/b / c/d == ad / bc
RATIONAL_OP( div, (a.num * b.denom), (a.denom * b.num))

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

static inline number_t as_rational_number( number_t val ){
	if ( val.type == TYPE_RATIONAL ){
		return val;
	} else {
		return (number_t){
			.type = TYPE_RATIONAL,
			.num   = val.s_int,
			.denom = 1,
		};
	}
}

static inline unsigned long find_gcd( unsigned long num, unsigned long denom ){
	unsigned long temp;

	while ( denom ){
		temp = denom;
		denom = num % denom;
		num = temp;
	}

	return num;
}

static inline number_t normalize_rational( number_t foo ){
	if ( foo.denom == 1 ){
		return (number_t){
			.type = TYPE_NUMBER,
			.s_int = foo.num,
		};

	} else if ( foo.num % foo.denom == 0 ) {
		return (number_t){
			.type = TYPE_NUMBER,
			.s_int = foo.num / (long int)foo.denom,
		};

	} else {
		long temp = find_gcd( foo.num, foo.denom );

		if ( temp != foo.denom ){
			foo.num   /= temp;
			foo.denom /= temp;
		}

		return foo;
	}
}

NUMBER_OP( add, + )
NUMBER_OP( sub, - )
NUMBER_OP( mul, * )
NUMBER_OP( div, / )

#endif
