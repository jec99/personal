CC=gcc
CFLAGS=-g3 -O3 -std=c99 -pedantic

TRIALS=4

all: testCollision

test: all
	for i in 0 1 2 3; do echo ./testCollision $$i $(TRIALS) && ./testCollision $$i $(TRIALS); done

testCollision: testCollision.o findCollision.o
	$(CC) $(CFLAGS) -o $@ $^

findCollision.o testCollision.o: findCollision.h

clean:
	$(RM) testCollision *.o
