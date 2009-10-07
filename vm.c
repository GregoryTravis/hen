// $Id$

#include <stdio.h>

#include "vm.h"

#include "yeah.h"

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
    Symbol(txt) { printf("%s", txt); }
    Pair(car, cdr) { printf("("); dump1(car); dump1_list(cdr); }
    Nil() { printf("()"); }
    end;
  }
}

void dump(yeah* y) {
  dump1(y);
  printf("\n");
}

int main(int argc, char** argv) {
  printf("vm!\n");

  dump(mksymbol("asdf"));
  dump(mkpair(mksymbol("asdf"), mksymbol("awer")));
  dump(mkpair(mksymbol("asdf"), mknil()));
  dump(mkpair(mksymbol("asdf"), mkpair(mksymbol("joe"), mknil())));
  dump(mkpair(mksymbol("asdf"), mkpair(mksymbol("joe"), mkpair(mksymbol("lap"), mknil()))));
  dump(mkpair(mkpair(mksymbol("asdf"), mksymbol("qwer")), mksymbol("zxcv")));
  dump(mknil());

  return 0;
}
