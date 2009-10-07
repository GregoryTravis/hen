// $Id$

#include <stdio.h>

#include "vm.h"

#include "yeah.h"

int main(int argc, char** argv) {
  printf("vm!\n");

  yeah* y = mksymbol("asdf");
  y = mkpair(mksymbol("asdf"), mksymbol("awer"));

  match(y) {
    Symbol(joe) {
      printf("sym!\n");
    }
    Pair(a, d) {
      printf("pair!\n");
    }
    end;
  }

  return 0;
}
