//search.c
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <ctype.h>
#include <stdint.h>

#include "search.h"

//Heap defs
#define DEFAULT_CAPACITY (1024)
#define Child(x, dir) (2 * (x) + 1 + (dir))
#define Parent(x) ((x - 1) / 2)

//Hash defs
#define INITIAL_SIZE (1024)
#define MAX_LOAD (0.5)
#define GROWTH_FACTOR (2)

typedef struct elt {
	int priority;
	int distance;
	struct position p;
} Elt;

typedef struct heap {
	int size;
	int capacity;
	Elt *elts;
} *Heap;

void float_down(int n, Elt *a, int pos) {
  Elt transfer = a[pos];
	for(;;) {
		if(Child(pos, 1) < n && a[Child(pos, 1)].priority < a[Child(pos, 0)].priority) {
			if(a[Child(pos, 1)].priority < transfer.priority) {
				a[pos] = a[Child(pos, 1)];
				pos = Child(pos, 1);
			} else {
				break;
			}
		} else if(Child(pos, 0) < n && a[Child(pos, 0)].priority < transfer.priority) {
			a[pos] = a[Child(pos, 0)];
			pos = Child(pos, 0);
		} else {
			break;
		}
	}
	a[pos] = transfer;
}

void float_up(Elt *a, int pos) {
	Elt transfer = a[pos];
	while(pos) {
		if(a[Parent(pos)].priority > transfer.priority) {
			a[pos] = a[Parent(pos)];
			pos = Parent(pos);
		} else {
			break;
		}
	}
	a[pos] = transfer;
}

int heap_empty(Heap h) {
	return (h->size == 0);
}

Heap heap_create(void) {
	Heap h = malloc(sizeof(*h));
	h->elts = malloc(DEFAULT_CAPACITY * sizeof(Elt));
	h->size = 0;
	h->capacity = DEFAULT_CAPACITY;
	return h;
}

void heap_insert(Heap h, Elt *e) {
	if(h->capacity < (h->size + 2)) {
		h->elts = realloc(h->elts, h->capacity * sizeof(Elt) * 2);
		h->capacity *= 2;
	}
    h->elts[h->size] = *e;
    float_up(h->elts, h->size);
    h->size++;
}

Elt *heap_delete_min(Heap h) {
	if(heap_empty(h)) return 0;
	Elt *retval = malloc(sizeof(Elt));
	*retval = h->elts[0];
	h->elts[0] = h->elts[h->size - 1];
	h->size--;
	float_down(h->size, h->elts, 0);
	return retval;
}

void heap_destroy(Heap h) {
  free(h->elts);
  free(h);
}

//Hash code
static unsigned int hash_function(int a, int b) {
  uint64_t key = (((uint64_t) b) << 32) + ((uint64_t) a);
	key = (~key) + (key << 18);
	key = key ^ (key >> 31);
	key = key * 21;
	key = key ^ (key >> 11);
	key = key + (key << 6);
	key = key ^ (key >> 22);
	return (unsigned int) key;
}

struct hash_elt {
	struct hash_elt *next;
	struct position p;
};

typedef struct hash {
	int size;
	int n;
	struct hash_elt **table;
} *Hash;

static void hash_grow(Hash h);
void hash_insert(Hash h, struct position p);

static Hash internal_hash_create(int size) {
	Hash h = malloc(sizeof(struct hash));
	h->size = size;
	h->n = 0;
	h->table = calloc(h->size, sizeof(struct hash_elt *));
	for(int i = 0; i < h->size; i++) { h->table[i] = 0; }
	return h;
}

Hash hash_create(void) {
	return internal_hash_create(INITIAL_SIZE);
}

void hash_destroy(Hash h) {
	struct hash_elt *e, *next;
	for(int i = 0; i < h->size; i++) {
		for(e = h->table[i]; e != 0; e = next) {
			next = e->next;
			free(e);
		}
	}
	free(h->table);
	free(h);
}

static void hash_grow(Hash h) {
	Hash h2;
	struct hash swap;
	struct hash_elt *e;
	h2 = internal_hash_create(h->size * GROWTH_FACTOR);
	for(int i = 0; i < h->size; i++) {
		for(e = h->table[i]; e; e = e->next) {
			hash_insert(h2, e->p);
		}
	}
	swap = *h;
	*h = *h2;
	*h2 = swap;
	hash_destroy(h2);
}

void hash_insert(Hash h, const struct position p) {
	struct hash_elt *e;
	int hashed = hash_function(p.x, p.y) % (h->size);
	e = malloc(sizeof(struct hash_elt));
	e->p.x = p.x;
	e->p.y = p.y;
	e->next = h->table[hashed];
	h->table[hashed] = e;
	h->n++;
	if(h->n >= h->size * MAX_LOAD) hash_grow(h);
}

int hash_contains(Hash h, const struct position p) {
	struct hash_elt *e;
	int hashed = hash_function(p.x, p.y) % (h->size);
	if(h->table[hashed] == 0) return 0;
	for(e = h->table[hashed]; e != 0; e = e->next) {
		if((e->p.x == p.x) && (e->p.y == p.y)) {
			return 1;
		}
	}
	return 0;
}

//Main alg
int search(struct position source, struct position target, int (*blocked)(struct position)) {
	int retval;
	int neighbors[8] = { -1, 0, 1, 0, 0, 1, 0, -1 };

	Heap open = heap_create();
	Hash closed = hash_create();

	Elt *e = malloc(sizeof(Elt));
	Elt *temp = malloc(sizeof(Elt));

	e->p.x = source.x;
	e->p.y = source.y;
	e->distance = 0;
	e->priority = abs(source.x - target.x) + abs(source.y - target.y);

	heap_insert(open, e);
	free(e);

	while((e = heap_delete_min(open))) {
		if(hash_contains(closed, e->p)) {
			free(e);
			continue;
		}

		if(e->p.x == target.x && e->p.y == target.y) {
			retval = e->distance;
	    	free(e);
	    	free(temp);
            heap_destroy(open);
            hash_destroy(closed);
            return retval;
        }

        hash_insert(closed, e->p);
	    	      
		for(int i = 0; i < 4; i++) {
	    	temp->p.x = e->p.x + neighbors[i];
	    	temp->p.y = e->p.y + neighbors[i + 4];
	    	temp->distance = e->distance + 1;
	    	temp->priority = abs(temp->p.x - target.x) + abs(temp->p.y - target.y) + temp->distance;

			if(hash_contains(closed, temp->p) || blocked(temp->p)) continue;
	    	heap_insert(open, temp);
		}

		free(e);
	}

	free(temp);
	heap_destroy(open);
	hash_destroy(closed);

	return NO_PATH;
}