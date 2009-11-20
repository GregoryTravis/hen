#include <string.h>

#include "blip.h"
#include "bmem.h"
#include "yeahlib.h"

void dump1(yeah* y);
void ldump(yeah* y);

#define OBJ(name) (_blargh_ ## name)
#define DEFOBJ(name) extern yeah* _blargh_ ## name

DEFOBJ(True);
DEFOBJ(False);
DEFOBJ(Closure);
DEFOBJ(Cons);
DEFOBJ(Nil);
DEFOBJ(buck);

yeah* list_syntax_unpreprocess(yeah* y);

yeah* list_syntax_unpreprocess_list1(yeah* y) {
  if (ispair(y) && car(y) == OBJ(Cons) && length(y) == 3) {
    return cons(list_syntax_unpreprocess(nth(y, 1)), list_syntax_unpreprocess_list1(nth(y, 2)));
  } else if (y == OBJ(Nil)) {
    return mknil();
  } else {
    return y;
  }
}

yeah* list_syntax_unpreprocess_list(yeah* y) {
  return cons(OBJ(buck), list_syntax_unpreprocess_list1(y));
}

yeah* list_syntax_unpreprocess(yeah* y) {
  if (ispair(y) && car(y) == OBJ(Cons) && length(y) == 3) {
    return list_syntax_unpreprocess_list(y);
  } else if (y == OBJ(Nil)) {
    return list_syntax_unpreprocess_list(y);
  } else if (ispair(y)) {
    return map(list_syntax_unpreprocess, y);
  } else {
    return y;
  }
}

yeah* unpreprocess(yeah* y) {
  return list_syntax_unpreprocess(y);
}

void dump1_list(yeah* y) {
  match(y) {
    Symbol(txt) { printf(" . "); dump1(y); printf(")"); }
    Number(txt) { printf(" . "); dump1(y); printf(")"); }
    Function(f, name) { printf(" . "); dump1(y); printf(")"); }
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
    Function(f, name) { printf("[func %s]", name); }
    Nil() { printf("()"); }
    end;
  }
}

void dumps(yeah* y) {
  y = unpreprocess(y);
  dump1(y);
}

void dump(yeah* y) {
  dumps(y);
  printf("\n");
}

void ldump(yeah* y) {
  dump1(y);
  printf("\n");
}

bool eq(yeah* a, yeah* b) {
  //printf("EQ "); dumps(a); printf(" "); dumps(b); printf("\n");
  if (a == b) {
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
  }

  err(("bool"));
  return false; // Unreached.
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
  return eq(a, b) ? OBJ(True) : OBJ(False);
}

bool isclosure(yeah* o) {
  return ispair(o) && car(o) == OBJ(Closure);
}

yeah* apply(yeah* f, yeah* args) {
  if (isfunction(f)) {
    return (*f->u.function.f)(args);
  } else if (isclosure(f)) {
    yeah* (*fun)(yeah*) = cadr(f)->u.function.f;
    yeah* closedOverArgs = cddr(f);
    return (*fun)(mklist2(closedOverArgs, args));
  }

  dump(f);
  dump(args);
  A(0);
  return NULL; // Unreached.
}
