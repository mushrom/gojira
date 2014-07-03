SRC=${wildcard src/*.c} ${wildcard src/libs/*.c} ${wildcard src/runtime/*.c}
OBJ=${SRC:.c=.o}
CFLAGS=-I"${shell pwd}"/include -g -Wall -O3

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
