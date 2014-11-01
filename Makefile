SRC = ${wildcard src/*.c} ${wildcard src/libs/*.c} ${wildcard src/runtime/*.c} ${wildcard src/parser/*.c}
MAINSRC = ${wildcard src/main/*.c} ${SRC} ${wildcard linenoise/linenoise.c}
OBJ = ${SRC:.c=.o}
MAINOBJ = ${MAINSRC:.c=.o}
CFLAGS=-I"${shell pwd}"/include -I. -p -g -Wall -O3

.c.o:
	@echo CC -c $< -o $@
	@${CC} ${CFLAGS} -c $< -o $@

out/gojira: ${MAINOBJ}
	@mkdir -p out
	@echo CC -o $@ ${MAINOBJ}
	@${CC} ${CFLAGS} -o $@ ${MAINOBJ}

out/libgojira.o: ${OBJ}
	@mkdir -p out
	@echo LD -r ${OBJ} -o $@
	@${LD} -r ${OBJ} -o $@

all: out/gojira

lib: out/libgojira.o

clean:
	-rm -rf out ${OBJ} ${MAINOBJ}

.PHONY: all
