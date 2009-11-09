/* $Id$ */

#include "a.h"
#include "yeahlib.h"

yeah* car(yeah* y) {
  A(ispair(y));
  return y->u.pair.car;
}

yeah* cdr(yeah* y) {
  A(ispair(y));
  return y->u.pair.cdr;
}

yeah* cadr(yeah* y) {
  return car(cdr(y));
}

yeah* cddr(yeah* y) {
  return cdr(cdr(y));
}
