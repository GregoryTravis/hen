// $Id$

#include <stdio.h>

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

#if 0
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
#endif

int main(int argc, char** argv) {
  printf("vm!\n");

  dump(mksymbol("asdf"));
  dump(mkpair(mksymbol("asdf"), mksymbol("awer")));
  dump(mkpair(mksymbol("asdf"), mknil()));
  dump(mkpair(mksymbol("asdf"), mkpair(mksymbol("joe"), mknil())));
  dump(mkpair(mksymbol("asdf"), mkpair(mksymbol("joe"), mkpair(mksymbol("lap"), mknil()))));
  dump(mkpair(mkpair(mksymbol("asdf"), mksymbol("qwer")), mksymbol("zxcv")));
  dump(mknil());

  printf("%d %d %d\n", issymbol(mksymbol("asdf")), ispair(mkpair(mksymbol("asdf"), mksymbol("awer"))), isnil(mknil()));
  printf("%d %d %d\n", ispair(mksymbol("asdf")), isnil(mkpair(mksymbol("asdf"), mksymbol("awer"))), issymbol(mknil()));

  return 0;
}
