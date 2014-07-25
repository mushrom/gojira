#ifndef _GOJIRA_SCHEME_RULES_H
#define _GOJIRA_SCHEME_RULES_H
#include <gojira/tokens.h>
#include <gojira/parser.h>

static rule_t *scheme_rules = 
	&(rule_t){
		.type = TYPE_OPEN_PAREN,
		.down = &(rule_t){
			.type = TYPE_CLOSE_PAREN,
			.ret  = TYPE_LIST,

		.next = &(rule_t){
			.type = TYPE_TOKEN_LIST,
			.down = &(rule_t){
				.type = TYPE_CLOSE_PAREN,
				.ret = TYPE_LIST,
			},

		.next = &(rule_t){
			.type = TYPE_BASE_TOKEN,
			.down = &(rule_t){
				.type = TYPE_PERIOD,
				.down = &(rule_t){
					.type = TYPE_BASE_TOKEN,
					.down = &(rule_t){
						.type = TYPE_CLOSE_PAREN,
						.ret = TYPE_PAIR,
					},
				},
			},
		}}},

	.next = &(rule_t){
		.type = TYPE_DEFINE,
		.down = &(rule_t){
			.type = TYPE_TOKEN_LIST,
			.ret = TYPE_DEFINE_EXPR,
		},

	/*
	.next = &(rule_t){
		.type = TYPE_LAMBDA,
		.down = &(rule_t){
			.type = TYPE_TOKEN_LIST,
			.ret = TYPE_PROCEDURE,
		},
	*/

	.next = &(rule_t){
		.type = TYPE_BASE_TOKEN,
		.ret = TYPE_TOKEN_LIST,

		.down = &(rule_t){
			.type = TYPE_TOKEN_LIST,
			.ret  = TYPE_TOKEN_LIST,
		},

	.next = &(rule_t){
		.type = TYPE_DEFINE_EXPR,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_LAMBDA,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_IF,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_DEF_SYNTAX,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_SYNTAX_RULES,
		.ret = TYPE_BASE_TOKEN,
	/*
	.next = &(rule_t){
		.type = TYPE_PROCEDURE,
		.ret = TYPE_BASE_TOKEN,
	*/

	.next = &(rule_t){
		.type = TYPE_BOOLEAN,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_NUMBER,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_CHAR,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_STRING,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_VECTOR,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_LIST,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_SYMBOL,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_QUOTED_TOKEN,
		.ret = TYPE_BASE_TOKEN,

	.next = &(rule_t){
		.type = TYPE_APOSTR,
		.down = &(rule_t){
			.type = TYPE_BASE_TOKEN,
			.ret = TYPE_QUOTED_TOKEN,
		},

	.next = &(rule_t){
		.type = TYPE_OCTOTHORPE,
		.down = &(rule_t){
			.type = TYPE_BASE_TOKEN,
			.ret = TYPE_VECTOR,
		},

	}}}}}}}}}}}}}}}}}};

#endif
