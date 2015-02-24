#include <gojira/parse_debug.h>

char *debug_strings[] = {
	"unspecified",
	"boolean",
	"number",
	"char",
	"string",
	"symbol",
	"pair",
	"list",
	"vector",

	"(",
	")",
	".",
	"'",
	"#",
	"*",
	",",
	";",

	"lambda",
	"if",
	"define-syntax",
	"syntax-rules",

	"base token",
	"token_list",
	"quoted",
	"procedure",

	"external procedure",
	"syntax",
	"variable reference",
	"If you see this something has gone terribly wrong",
};

char *type_str( type_t type ){
	return debug_strings[ type ];
}
