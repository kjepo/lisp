#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "hashtab.h"

#define HASHTABSIZ 1000

struct {
  char *id;
} hashtab[HASHTABSIZ];

int hash(char *name) {
  return name[0] % HASHTABSIZ;
}

int lookup(char *name) {
  int h = hash(name);
  for (int i = 0; i < HASHTABSIZ; i++) {
    if (hashtab[h].id == 0) {
      char *p = malloc(strlen(name) + 1);
      strcpy(p, name);
      hashtab[h].id = p;
      return h;
    } else if (!strcmp(hashtab[h].id, name)) {
      return h;
    } else
      h = (h+1) % HASHTABSIZ;
  }
  fprintf(stderr, "hashtable is full!\n");
  dump_hashtab();
  exit(1);
}

char *find(int index) {
  //  printf("find: index = %d\n", index);
  //  printf("hashtab[index].id = %x\n", hashtab[index].id);
  return hashtab[index].id;
}


void dump_hashtab() {
  printf("   HASH TABLE:\n");
  for (int i = 0; i < HASHTABSIZ; i++) {
    printf("%4d: \"%s\" ", i, (hashtab[i].id ? hashtab[i].id : ""));
    /*
    if (hashtab[i].id) {
      for (char *p = hashtab[i].id; *p; p++)
	printf("[%c]", *p);
      printf(" %d chars", (int) strlen(hashtab[i].id));
    }
    */
    printf("\n");
  }
}

void testhashtab() {
  int i1 = lookup("CAR");
  int i2 = lookup("CDR");
  int i3 = lookup("ATOM");
  int i4 = lookup("EQ");
  int i5 = lookup("NIL");
  int i6 = lookup("LAMBDA");
  int i7 = lookup("CAR");
  dump_hashtab();
  if (i1 != i7) {
    fprintf(stderr, "i1=%d and i7=%d are different\n", i1, i7);
    exit(1);
  }
}

