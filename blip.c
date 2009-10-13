#include "blip.h"

void dump(yeah* y);
void dump1(yeah* y);

void dump1_list(yeah* y) {
  match(y) {
    Symbol(txt) { printf(" . "); dump1(y); printf(")"); }
    Pair(car, cdr) { printf(" "); dump1(car); dump1_list(cdr); }
    Nil() { printf(")"); }
    end;
  }
}

void dump1(yeah* y) {
  match (y) {
    Symbol(txt) { printf("(Lit %s)", txt); }
    Pair(car, cdr) { printf("("); dump1(car); dump1_list(cdr); }
    Nil() { printf("()"); }
    end;
  }
}

void dumps(yeah* y) {
  dump1(y);
}

void dump(yeah* y) {
  dumps(y);
  printf("\n");
}

bool samesymbol(yeah* a, yeah* b) {
  A(issymbol(a));
  A(issymbol(b));
  return !strcmp(a->u.symbol.txt, a->u.symbol.txt);
}
