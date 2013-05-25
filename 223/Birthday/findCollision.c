#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <limits.h>

#include "findCollision.h"

#define MAX_HEIGHT (32)

//RAND_MAX on my machine is UINT_MAX / 2, rounded down
//So use 2 * rand() + rand() & 1 to get in the right range
//NOTE: value is x, key is f(x). Backwards, I know, but that's how it happened.

typedef struct skiplist {
    unsigned int key;
    unsigned int value;
    int height;                /* number of next pointers */
    struct skiplist *next[1];  /* first of many */
} *Skiplist;

static int chooseHeight(void) {
	int i;
    for(i = 1; i < MAX_HEIGHT && rand() % 2 == 0; i++);
    return i;
}

static Skiplist skiplistCreateNode(unsigned int key, unsigned int value, int height) {
    Skiplist s = malloc(sizeof(struct skiplist) + sizeof(struct skiplist *) * (height - 1));
    s->key = key;
    s->height = height;
    s->value = value;
    return s;
}

static Skiplist skiplistCreate(void) {
    Skiplist s = skiplistCreateNode(0, 0, MAX_HEIGHT);
    s->height = 1;
    for(int i = 0; i < MAX_HEIGHT; i++) {
        s->next[i] = 0;
    }
    return s;
}

static void skiplistDestroy(Skiplist s) {
    Skiplist next;
    while(s) {
        next = s->next[0];
        free(s);
        s = next;
    }
}

static Skiplist skiplistSearch(Skiplist s, unsigned int key) {
    for(int level = s->height - 1; level >= 0; level--) {
        while(s->next[level] && s->next[level]->key <= key) {
            s = s->next[level];
        }
    }
    return s;
}

static void skiplistInsert(Skiplist s, unsigned int key, unsigned int value) {
    int level;
    Skiplist elt;
    elt = skiplistCreateNode(key, value, chooseHeight());
    if(elt->height > s->height) s->height = elt->height;

    for(level = s->height - 1; level >= elt->height; level--) {
        while(s->next[level] && s->next[level]->key < key) {
            s = s->next[level];
        }
    }

    for(; level >= 0; level--) {
        while(s->next[level] && s->next[level]->key < key) {
            s = s->next[level];
        }
        elt->next[level] = s->next[level];
        s->next[level] = elt;
    }
}

static void skiplistDelete(Skiplist s, unsigned int key) {
    int level;
    Skiplist target;
    target = s;

    for(level = s->height - 1; level >= 0; level--) {
        while(target->next[level] && target->next[level]->key < key) {
            target = target->next[level];
        }
    }
    
    target = target->next[0];
    if(target == 0 || target->key != key) return;

    for(level = s->height - 1; level >= 0; level--) {
        while(s->next[level] && s->next[level]->key < key) {
            s = s->next[level];
        }
        if(s->next[level] == target) {
            s->next[level] = target->next[level];
        }
    }
    free(target);
}

void findCollision(unsigned int (*f)(unsigned int), unsigned int *x1, unsigned int *x2) {
	srand(time(NULL));

	unsigned int try, out, temp, mask;
	Skiplist values, elt;

	mask = 0x7fffffff;
	values = skiplistCreate();
	
	for(temp = (unsigned int) rand(); 1; temp = (unsigned int) rand()) {
		try = 2 * temp + (temp & 1);
		out = (*f)(try) & mask;
		if((elt = skiplistSearch(values, out))->value == try) continue;
		if(elt->key == out) {
			*x1 = try;
			*x2 = elt->value;
			skiplistDestroy(values);
			return;
		} else skiplistInsert(values, out, try);
	}
}