/* Given a function int f(int), finds values x1 != x2 s.t. f(x1) & 0x7fffffff == f(x2) & 0x7fffffff */
void fincCollision(unsigned int(*f)(unsigned int), unsigned int *x1, unsigned int *x2);