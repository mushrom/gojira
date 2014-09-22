SRC = ${wildcard src/*.c} ${wildcard src/libs/*.c} ${wildcard src/runtime/*.c}
SRC+= ${wildcard linenoise/linenoise.c}
OBJ = ${SRC:.c=.o}
CFLAGS=-I"${shell pwd}"/include -I. -p -g -Wall -O3

.c.o:
	@echo CC -c $< -o $@
	@${CC} ${CFLAGS} -c $< -o $@

out/gojira: ${OBJ}
	@mkdir -p out
	@echo CC -o $@ ${OBJ}
	@${CC} ${CFLAGS} -o $@ ${OBJ}

all: out/gojira

clean:
	-rm -rf out ${OBJ}

.PHONY: all
