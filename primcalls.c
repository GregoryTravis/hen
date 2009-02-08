#include "vor.h"

yeah* primcall_plus(yeah* e) {
  yeah* a, *b;
  A(hmatch_list2(e, &a, &b));
  return integer(getint(a) + getint(b));
}

void install_primcalls(void) {
  add_primcall("plus", &primcall_plus);
}
