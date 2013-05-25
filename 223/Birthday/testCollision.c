#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>

#include "findCollision.h"

/* how many times f was called */
static int Count = 0; 

static unsigned int
identity(unsigned int x)
{
    Count++;
    return x;
}

static unsigned int
shift(unsigned int x)
{
    Count++;
    return x >> 2;
}

static unsigned int
square(unsigned int x)
{
    Count++;
    return x*x;
}

#define ROUNDS (256)

static unsigned int
slow(unsigned int x)
{
    int i;
    unsigned short lo;
    unsigned short hi;

    Count++;

    /* basic Feistel network */
    for(i = 0; i < ROUNDS; i++) {
        lo = x & 0xffff;
        hi = x >> 16;

        /* this is pretty arbitrary */
        lo ^= (hi * (2*i+1) ^ (hi << 5) & (hi >> 3));

        x = (lo << 16) | hi;
    }

    return x;
}

#define NUM_FUNCTIONS (4)

/* table of functions so we can specify them by index */
static unsigned int (*functions[NUM_FUNCTIONS])(unsigned int)  = {
    identity,
    shift,
    square,
    slow
};

int
main(int argc, char **argv)
{
    unsigned int x1;
    unsigned int x2;
    int functionIndex;
    int numTrials;
    int i;

    if(argc != 3) {
        fprintf(stderr, "Usage: %s function-index trials\n", argv[0]);
        return 1;
    }

    functionIndex = atoi(argv[1]);
    if(functionIndex < 0 || functionIndex >= NUM_FUNCTIONS) {
        fprintf(stderr, "function index out of range [%d, %d)\n", 0, NUM_FUNCTIONS);
        return 2;
    }

    numTrials = atoi(argv[2]);
    if(numTrials <= 0) {
        fprintf(stderr, "specify at least one trial\n", 0, NUM_FUNCTIONS);
        return 2;
    }


    srand(time(0));

    for(i = 0; i < numTrials; i++) {
        Count = 0;
        findCollision(functions[functionIndex], &x1, &x2);
        printf("Found collision %08x %08x in %d probes\n", x1, x2, Count);
    }

    return 0;
}
