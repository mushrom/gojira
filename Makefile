SRC=${wildcard *.c}
OBJ=${SRC:.c=.o}

.c.o:
	${CC} -c $<

gojira: ${OBJ}
	${CC} -o $@ ${OBJ}

clean:
	-rm gojira *.o

.PHONY: gojira
