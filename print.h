#ifndef DEBUGH
#define DEBUGH

void display(Obj expr);
void display_registers(char *where);
void dump_memory();
void printBits(size_t const size, void const * const ptr);

#endif
