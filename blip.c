#include "blip.h"

#include "yeahlib.h"

void dump(yeah* y);
void dump1(yeah* y);

void dump1_list(yeah* y) {
  match(y) {
    Symbol(txt) { printf(" . "); dump1(y); printf(")"); }
    Number(txt) { printf(" . "); dump1(y); printf(")"); }
    Pair(car, cdr) { printf(" "); dump1(car); dump1_list(cdr); }
    Nil() { printf(")"); }
    end;
  }
}

void dump1(yeah* y) {
  match (y) {
    Symbol(txt) { printf("%s", txt); }
    Pair(car, cdr) { printf("("); dump1(car); dump1_list(cdr); }
    Number(n) { printf("%g", n); }
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

bool eq(yeah* a, yeah* b) {
  // TODO: should intern this
  if (issymbol(a) && issymbol(b) && !strcmp(a->u.symbol.txt, b->u.symbol.txt)) {
    return true;
  } else if (isnumber(a) && isnumber(b) && (a->u.number.d == b->u.number.d)) {
    return true;
  } else if (isnil(a) && isnil(b)) {
    return true;
  } else {
    return false;
  }
}

void listmatch1(yeah* list, yeah** a) {
  A(ispair(list));
  *a = car(list);
  A(isnil(cdr(list)));
}

void listmatch2(yeah* list, yeah** a, yeah** b) {
  A(ispair(list));
  *a = car(list);
  listmatch1(cdr(list), b);
}

binop_def(plus, +)
binop_def(minus, -)
