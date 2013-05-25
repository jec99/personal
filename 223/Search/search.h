#define NO_PATH (-1)

struct position {
	int x;
	int y;
};

int search(struct position source, struct position target, int (*blocked)(struct position));