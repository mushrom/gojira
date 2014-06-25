SRC=${wildcard *.c}
OBJ=${SRC:.c=.o}
CFLAGS=-I"${shell pwd}"/include -g

.c.o:
	${CC} ${CFLAGS} -c $<

gojira: ${OBJ}
	${CC} ${CFLAGS} -o $@ ${OBJ}

clean:
	-rm gojira *.o

.PHONY: gojira
