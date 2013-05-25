#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "search.h"

static int
blockNever(struct position pos)
{
    return 0;
}

static int
blockAlways(struct position pos)
{
    return 1;
}

static int
blockEven(struct position pos)
{
    return (pos.x + pos.y) % 2 == 0;
}

#define BARRICADE_HEIGHT (200)

static int
blockBarricades(struct position pos)
{
    return (pos.x % 2 == 1) && (pos.y > -BARRICADE_HEIGHT) && (pos.y < BARRICADE_HEIGHT);
}

#define PERCOLATION_BLOCK_INVERSE_PROBABILITY (3) 

static int
blockPercolation(struct position pos)
{
    int i;

    int h[2];
    int temp;

    if(pos.x == 0 && pos.y == 0) {
        return 0;
    }

    h[0] = pos.x;
    h[1] = pos.y;

    for(i = 0; i < 8; i++) {
        temp = h[0];
        h[0] = h[0] ^ (h[0] * 3 + (h[1] >> 5) + (h[1] << 7));
        h[1] = temp;
    }

    return h[0] % PERCOLATION_BLOCK_INVERSE_PROBABILITY == 0;
}

int
main(int argc, char **argv)
{
    struct position s;
    struct position t;

    if(argc != 1) {
        fprintf(stderr, "Usage: %s\n", argv[0]);
        return 1;
    }

    s.x = s.y = 0;
    t.x = t.y = 2;

    assert(search(s, t, blockNever) == 4);
    assert(search(s, t, blockAlways) == NO_PATH);
    assert(search(s, t, blockEven) == NO_PATH);

    t.x = t.y = 1000;

    assert(search(s, t, blockNever) == 2000);
    assert(search(s, t, blockAlways) == NO_PATH);
    assert(search(s, t, blockEven) == NO_PATH);

    s.x = s.y = 1;

    assert(search(s, s, blockNever) == 0);
    assert(search(s, t, blockEven) == NO_PATH);

    s.x = s.y = 0;
    t.x = 2;
    t.y = 0;

    assert(search(s, t, blockNever) == 2);
    assert(search(s, t, blockBarricades) == 2*BARRICADE_HEIGHT + 2);

    s.x = 1;
    s.y = 2000;
    
    t.x = 1;
    t.y = -2000;

    assert(search(s, t, blockNever) == 4000);
    assert(search(s, t, blockBarricades) == 4002);

    s.x = s.y = 0;
    t.x = 200;
    t.y = 0;

    assert(search(s, t, blockBarricades) == 2*BARRICADE_HEIGHT + 200);

    s.x = s.y = 0;
    t.x = 0;
    t.y = 1000;

    assert(search(s, t, blockPercolation) == 1396);

    return 0;
}
