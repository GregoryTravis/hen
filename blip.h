#ifndef __blip_h_
#define __blip_h_

#include "a.h"
#include "yeah.h"

bool eq(yeah* a, yeah* b);
bool isbooltrue(yeah* b, yeah* t, yeah* f);

yeah* mklist0(void);
yeah* mklist1(yeah* a);
yeah* mklist2(yeah* a, yeah* b);
yeah* mklist3(yeah* a, yeah* b, yeah* c);
yeah* mklist4(yeah* a, yeah* b, yeah* c, yeah* d);

void dump(yeah* y);
void dumps(yeah* y);
bool samesymbol(yeah* a, yeah* b);

#define binop_decl(_name, _op) yeah* __##_name(yeah* args)
#define binop_def(_name, _op) binop_decl(_name, _op) { \
  yeah* a; yeah* b; \
  listmatch2(args, &a, &b); \
  if (isinteger(a) && isinteger(b)) { \
    return mkinteger(a->u.integer.i _op b->u.integer.i); \
  } else if (isinteger(a) && isflote(b)) { \
    return mkflote(a->u.integer.i _op b->u.flote.d); \
  } else if (isflote(a) && isinteger(b)) { \
    return mkflote(a->u.flote.d _op b->u.integer.i); \
  } else if (isflote(a) && isflote(b)) { \
    return mkflote(a->u.flote.d _op b->u.flote.d); \
  } \
  dump(a); \
  dump(b); \
  printf("%d %d\n", isinteger(a), isinteger(b)); \
  err(("Bad binop args")); \
  return NULL; \
}

binop_decl(plus, +);
binop_decl(minus, -);
binop_decl(times, *);
binop_decl(div, /);

yeah* __eqeq(yeah* args);

typedef yeah* (*primfun)(yeah*);

typedef struct {
  char* name;
  primfun fun;
} funly;

yeah* apply(yeah* f, yeah* args);
yeah* driver(yeah* command);

yeah* __prim_putchar(yeah* i);
yeah* __prim_getchar(yeah* nil);

//#define mmkpair(a, d) ({yeah* y = BNEW(yeah); y->tag = TAG_pair; y->u.pair.car = (a); y->u.pair.cdr = (d); y;})

#endif // __blip_h_
