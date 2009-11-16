#include <string.h>

#include "blip.h"

#include "yeahlib.h"

void dump(yeah* y);
void dump1(yeah* y);

yeah _True_ = { TAG_symbol, { .symbol = "True" } }, *_True = &_True_;
yeah _False_ = { TAG_symbol, { .symbol = "False" } }, *_False = &_False_;

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
  //printf("EQ "); dumps(a); printf(" "); dumps(b); printf("\n");
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

bool isbooltrue(yeah* b, yeah* t, yeah* f) {
  if (eq(b, t)) {
    return true;
  } else if (eq(b, f)) {
    return false;
  } else {
    err(("bool"));
  }
}

yeah* mklist0(void) {
  return mknil();
}

yeah* mklist1(yeah* a) {
  return mkpair(a, mknil());
}

yeah* mklist2(yeah* a, yeah* b) {
  return mkpair(a, mklist1(b));
}

yeah* mklist3(yeah* a, yeah* b, yeah* c) {
  return mkpair(a, mklist2(b, c));
}

yeah* mklist4(yeah* a, yeah* b, yeah* c, yeah* d) {
  return mkpair(a, mklist3(b, c, d));
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

void listmatch3(yeah* list, yeah** a, yeah** b, yeah** c) {
  A(ispair(list));
  *a = car(list);
  listmatch2(cdr(list), b, c);
}

#define ARGS2() yeah *a, *b; listmatch2(args, &a, &b)

binop_def(plus, +)
binop_def(minus, -)
binop_def(times, *)
binop_def(div, /)

yeah* __eqeq(yeah* args) {
  ARGS2();
  return eq(a, b) ? _True : _False;
}

bool isthissymbol(yeah* o, char *name) {
  return issymbol(o) && !strcmp(o->u.symbol.txt, name);
}

bool isclosure(yeah* o) {
  return ispair(o) && isthissymbol(car(o), "Closure");
}

primfun funlookup(yeah* sym) {
  funly* here = &funlies[0];

  A(issymbol(sym));

  int i;
  while (1) {
    if (here->name == NULL) {
      break;
    } else if (!strcmp(here->name, sym->u.symbol.txt)) {
      return here->fun;
    } else {
      here++;
    }
  }

  dump(sym);
  A(0);
}

yeah* apply(yeah* f, yeah* args) {
  if (issymbol(f)) {
    return (*(funlookup(f)))(args);
  } else if (isclosure(f)) {
    yeah* name = cadr(f);
    yeah* closedOverArgs = cddr(f);
    return (*(funlookup(name)))(mklist2(closedOverArgs, args));
  } else {
    A(0);
  }
}
