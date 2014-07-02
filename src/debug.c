#include <gojira/parse_debug.h>

char *debug_strings[] = {
	"null",
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

	"base token",
	"token_list",
	"quoted",
};

char *type_str( type_t type ){
	return debug_strings[ type ];
}
