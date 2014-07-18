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

	"define",
	"lambda",

	"base token",
	"token_list",
	"quoted",
	"definition",
	"procedure",

	"external procedure",
};

char *type_str( type_t type ){
	return debug_strings[ type ];
}
