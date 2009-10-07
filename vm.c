// $Id$

#include <stdio.h>

#include "vm.h"

#include "a.h"
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

void write_and_read(yeah* y) {
  FILE* fp = fopen("outout", "wb");
  A(fp);
  serialize(fp, y);
  fclose(fp);

  fp = fopen("outout", "r");
  A(fp);
  yeah* yy = unserialize(fp);
  fclose(fp);

  dump(y);
  dump(yy);

  unlink("outout");
}

int main(int argc, char** argv) {
  printf("vm!\n");

  write_and_read(mknil());
  write_and_read(mksymbol("asdf"));
  write_and_read(mkpair(mksymbol("asdf"), mkpair(mksymbol("joe"), mkpair(mksymbol("lap"), mknil()))));
  write_and_read(mkpair(mkpair(mksymbol("asdf"), mksymbol("qwer")), mksymbol("zxcv")));

  return 0;
}
