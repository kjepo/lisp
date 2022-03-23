#ifndef DEBUGH
#define DEBUGH

void display(Obj expr);
void display2(Obj expr, int, int);
void display_registers(char *where);
void dump_memory();
void printBits(size_t const size, void const * const ptr);

#endif
