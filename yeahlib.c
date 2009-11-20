/* $Id$ */

#include <string.h>

#include "a.h"
#include "bmem.h"
#include "yeahlib.h"

yeah* car(yeah* y) {
  A(ispair(y));
  return y->u.pair.car;
}

yeah* cdr(yeah* y) {
  A(ispair(y));
  return y->u.pair.cdr;
}

yeah* cons(yeah* a, yeah* d) {
  return mkpair(a, d);
}

yeah* cadr(yeah* y) {
  return car(cdr(y));
}

yeah* cddr(yeah* y) {
  return cdr(cdr(y));
}

int length(yeah* y) {
  if (isnil(y)) {
    return 0;
  } else {
    return 1 + length(cdr(y));
  }
}

yeah* nth(yeah* y, int n) {
  if (n == 0) {
    return car(y);
  } else {
    return nth(cdr(y), n - 1);
  }
}

yeah* map(yeah* (*f)(yeah*), yeah* list) {
  if (isnil(list)) {
    return list;
  } else {
    A(ispair(list));
    return cons(f(car(list)), map(f, cdr(list)));
  }
}

bool isthissymbol(yeah* o, char *name) {
  return issymbol(o) && !strcmp(o->u.symbol.txt, name);
}
