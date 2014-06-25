SRC=${wildcard *.c}
OBJ=${SRC:.c=.o}
CFLAGS=-I"${shell pwd}"/include -g -Wall

.c.o:
	@echo CC -c $<
	@${CC} ${CFLAGS} -c $<

out/gojira: ${OBJ}
	@mkdir -p out
	@echo CC -o $@ ${OBJ}
	@${CC} ${CFLAGS} -o $@ ${OBJ}

all: out/gojira

clean:
	-rm -rf out *.o

.PHONY: all
