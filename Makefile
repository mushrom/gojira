SRC = ${wildcard src/*.c} ${wildcard src/libs/*.c} ${wildcard src/runtime/*.c} ${wildcard src/parser/*.c}
SRC+= ${wildcard src/runtime/builtins/*.c}
MAINSRC = ${wildcard src/main/*.c} ${SRC} ${wildcard linenoise/linenoise.c}
OBJ = ${SRC:.c=.o}
MAINOBJ = ${MAINSRC:.c=.o}
CFLAGS=-I"${shell pwd}"/include -I. -g -Wall -O3 -std=c11 -pedantic -D_DEFAULT_SOURCE
PREFIX=/

.PHONY: all
all: out/gojira

include/gojira/config.h:
	sed 's#SOME_PREFIX_HERE#${PREFIX}/share/gojira#' < include/gojira/config.h.tmp > include/gojira/config.h

.c.o:
	@echo CC -c $< -o $@
	@${CC} ${CFLAGS} -c $< -o $@

out/gojira: include/gojira/config.h ${MAINOBJ}
	@mkdir -p out
	@echo CC -o $@ ${MAINOBJ}
	@${CC} ${CFLAGS} -o $@ ${MAINOBJ}

out/libgojira.o: ${OBJ}
	@mkdir -p out
	@echo LD -r ${OBJ} -o $@
	@${LD} -r ${OBJ} -o $@

lib: out/libgojira.o

clean:
	-rm -rf out ${OBJ} ${MAINOBJ}
	-rm -f include/gojira/config.h

.PHONY: test
test:
	@cd tests; sh dotests.sh

install: out/gojira
	mkdir -p "${PREFIX}/bin" "${PREFIX}/share"
	cp out/gojira "${PREFIX}/bin"
	chmod 755 "${PREFIX}/bin/gojira"
	cp -rv ./libs "${PREFIX}/share/gojira"
	echo "(define modpath \"${PREFIX}/share/gojira/\")" >> "${PREFIX}/share/gojira/base.scm"

uninstall:
	rm -rf "${PREFIX}/bin/gojira"
	rm -rf "${PREFIX}/share/gojira"
