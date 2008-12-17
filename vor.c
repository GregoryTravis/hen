#include <stdio.h>
#include <string.h>

static int count=0;

#include "a.h"
#include "mem.h"
#include "spew.h"

static int trace = 0;
static int trace_env_too = 0;
static int max_trace_show = 6;

typedef enum {
  LAMBDA,
  CLOSURE,
  THUNK,
  INTEGER,
  SYMBOL,
  PAIR,
  APP,
  CSYMBOL,
//  NIL,
//  TRUE,
//  FALSE,
} tag;

typedef struct yeah yeah;

yeah *Nil, *True, *False;
yeah* globals;

struct yeah {
  tag t;
  union {

    struct {
      yeah* arg;
      yeah* body;
    } lambda;

    struct {
      yeah* lambda;
      yeah* env;
    } closure;

    struct {
      yeah* exp;
      yeah* env;
    } thunk;

    struct {
      int i;
    } integer;

    struct {
      char* s;
    } symbol;

    struct {
      yeah* car;
      yeah* cdr;
    } pair;

    struct {
      yeah* f;
      yeah* arg;
    } app;

    struct {
      char* s;
    } csymbol;

    struct {
      yeah* a;
      yeah* b;
    } generic_pointer_pair;
  } u;
};

yeah* newyeah(void) {
  yeah* y = NEW(yeah);
  A(y);
  return y;
}

yeah* closure(yeah* lambda, yeah* env) {
  yeah* y = newyeah();
  y->t = CLOSURE;
  y->u.closure.lambda = lambda;
  y->u.closure.env = env;
  return y;
}

yeah* lambda(yeah* arg, yeah* body) {
  yeah* y = newyeah();
  y->t = LAMBDA;
  y->u.lambda.arg = arg;
  y->u.lambda.body = body;
  return y;
}

yeah* thunk(yeah* exp, yeah* env) {
  yeah* y = newyeah();
  y->t = THUNK;
  y->u.thunk.exp = exp;
  y->u.thunk.env = env;
  return y;
}

yeah* symbol(char* s) {
  yeah* y = newyeah();
  y->t = SYMBOL;
  y->u.symbol.s = s;
  return y;
}

yeah* csymbol(char* s) {
  yeah* y = newyeah();
  y->t = CSYMBOL;
  y->u.symbol.s = s;
  return y;
}

yeah* pair(yeah* car, yeah* cdr) {
  yeah* y = newyeah();
  y->t = PAIR;
  y->u.pair.car = car;
  y->u.pair.cdr = cdr;
  return y;
}

yeah* integer(int i) {
  yeah* y = newyeah();
  y->t = INTEGER;
  y->u.integer.i = i;
  return y;
}

yeah* app(yeah* f, yeah* arg) {
  yeah* y = newyeah();
  y->t = APP;
  y->u.app.f = f;
  y->u.app.arg = arg;
  return y;
}

yeah* nil(void) {
  return Nil;
}

/*
yeah* ntrue(void) {
  yeah* y = newyeah();
  y->t = TRUE;
  return y;
}

yeah* nfalse(void) {
  yeah* y = newyeah();
  y->t = FALSE;
  return y;
}
*/

bool nilp(yeah* e) {
  return (e->t == CSYMBOL || e->t == SYMBOL) && !strcmp(e->u.csymbol.s, "Nil");
}

void dump(yeah* y) {
  switch (y->t) {
  case INTEGER: printf( "%d", y->u.integer.i ); break;
  case SYMBOL: printf( "%s", y->u.symbol.s ); break;
  case CSYMBOL: printf( "'%s", y->u.symbol.s ); break;
  case PAIR:
    printf( "(P " );
    dump(y->u.pair.car);
    printf( " " );
    dump(y->u.pair.cdr);
    printf( ")" );
    break;
  case THUNK:
    printf( "(@ ");
    dump(y->u.thunk.exp);
    printf(")");
    //dump(y->u.thunk.env);
    break;
  case CLOSURE:
    printf( "($ ");
    dump(y->u.closure.lambda);
    printf( " ");
    dump(y->u.closure.env);
    printf( ")");
    break;
  case LAMBDA:
    printf( "(/. ");
    dump(y->u.lambda.arg);
    printf( " ");
    dump(y->u.lambda.body);
    printf( ")");
    break;
  case APP:
    printf( "(");
    dump(y->u.app.f);
    printf( " ");
    dump(y->u.app.arg);
    printf( ")");
    break;
//  case NIL: printf("Nil"); break;
//  case TRUE: printf("True"); break;
//  case FALSE: printf("False"); break;
  default: err(("%d\n", y->t)); break;
  }
}

void dumpn(yeah* y) {
  dump(y);
  putchar('\n');
}

yeah* store_global(char *s, yeah* v) {
  globals = pair(pair(symbol(s), v), globals);
}

yeah* lookup_in_env(char* s, yeah* env) {
  if (nilp(env)) {
    return NULL;
  } else {
    A(env->t == PAIR);
    A(env->u.pair.car->t == PAIR);
    A(env->u.pair.car->u.pair.car->t == SYMBOL);
    if (!strcmp(s, env->u.pair.car->u.pair.car->u.symbol.s)) {
      return env->u.pair.car->u.pair.cdr;
    } else {
      return lookup_in_env(s, env->u.pair.cdr);
    }
  }
}

yeah* lookup(char *s, yeah* local_env) {
  yeah* v = lookup_in_env(s, local_env);
  if (v == NULL) {
    v = lookup_in_env(s, globals);
  }
  if (v == NULL) {
    err(("No such variable %s\n", s));
  }
}

yeah* freeze(yeah* e, yeah* env) {
  if (e->t == THUNK) {
    return e;
  } else {
    return thunk(e, env);
  }
}

bool equal(yeah* a, yeah* b) {
  if (a->t != b->t) {
    return false;
  }

  switch (a->t) {
  case INTEGER: return a->u.integer.i == b->u.integer.i;
 break;
  case SYMBOL: return !strcmp(a->u.symbol.s, b->u.symbol.s);
 break;
  case CSYMBOL: return !strcmp(a->u.csymbol.s, b->u.csymbol.s);
 break;
  NIL: return true;
 break;
  case LAMBDA: case CLOSURE: case THUNK: case PAIR: case APP:
    return
      equal(a->u.generic_pointer_pair.a, b->u.generic_pointer_pair.a) &&
      equal(a->u.generic_pointer_pair.b, b->u.generic_pointer_pair.b);
    break;
  default: printf("DIE %d\n", a->t); A(0); break;
  }
  A(0);
}

#define ISSYM(e) ((e)->t == SYMBOL && !strcmp((e)->u.symbol.s, (_s)))

#define APPF(e) ((e)->u.app.f)
#define APPARG(e) ((e)->u.app.arg)

yeah* evl_step(yeah* e, yeah* env);
yeah* evl_fully(yeah* e, yeah* env);

yeah* evl_step_(yeah* e, yeah* env) {
count++;
  if (e->t == APP && e->u.app.f->t == SYMBOL) {
    char* f = e->u.app.f->u.symbol.s;
    yeah* a = e->u.app.arg;
    if (!strcmp(f, "pair?")) {
      yeah* aa = evl_fully(a, env);
      return aa->t == PAIR ? True : False;
    } else if (!strcmp(f, "car")) {
      yeah* aa = evl_fully(a, env);
      A(aa->t == PAIR);
      return aa->u.pair.car;
    } else if (!strcmp(f, "cdr")) {
      yeah* aa = evl_fully(a, env);
      A(aa->t == PAIR);
      return aa->u.pair.cdr;
    } else {
      //err(("Unknown primitive %s\n", f));
      //evl_step_(app(e->u.app.f, evl_step(a, env)), env);
      return evl_step_(app(lookup(f, env), a), env);
    }
  } if (e->t == APP && e->u.app.f->t == APP && e->u.app.f->u.app.f->t == SYMBOL) {
    char* f = e->u.app.f->u.app.f->u.symbol.s;
    yeah* a = e->u.app.f->u.app.arg;
    yeah* b = e->u.app.arg;
    if (!strcmp(f, "+")) {
      a = evl_fully(a, env);
      b = evl_fully(b, env);
      A(a->t == INTEGER && b->t == INTEGER);
      return integer(a->u.integer.i + b->u.integer.i);
    } else if (!strcmp(f, "-")) {
      a = evl_fully(a, env);
      b = evl_fully(b, env);
      A(a->t == INTEGER && b->t == INTEGER);
      return integer(a->u.integer.i - b->u.integer.i);
    } else if (!strcmp(f, "*")) {
      a = evl_fully(a, env);
      b = evl_fully(b, env);
      A(a->t == INTEGER && b->t == INTEGER);
      return integer(a->u.integer.i * b->u.integer.i);
    } else if (!strcmp(f, "cons")) {
      return pair(freeze(a, env), freeze(b, env));
    } else if (!strcmp(f, "==")) {
      return equal(evl_fully(a, env), evl_fully(b, env)) ? True : False;
    } else {
      //err(("Unknown primitive %s\n", f));
      return evl_step_(app(app(lookup(f, env), a), b), env);
    }
  } else if (e->t == APP && e->u.app.f->t == CLOSURE) {
    //printf("BIND %s\n", e->u.app.f->u.closure.lambda->u.lambda.arg->u.symbol.s); dumpn(e->u.app.arg);
    return evl_step(
      e->u.app.f->u.closure.lambda->u.lambda.body,
      pair(
        pair(
          e->u.app.f->u.closure.lambda->u.lambda.arg,
          e->u.app.arg),
        e->u.app.f->u.closure.env));
  } else if (e->t == APP && APPF(e)->t == APP && APPF(APPF(e))->t == APP && APPF(APPF(APPF(e)))->t == SYMBOL
    && !strcmp(APPF(APPF(APPF(e)))->u.symbol.s, "if")) {
    yeah* b = APPARG(APPF(APPF(e)));
    yeah* th = APPARG(APPF(e));
    yeah* el = APPARG(e);
    /*printf("IF\n");
    dumpn(e);
    dumpn(b);
    dumpn(th);
    dumpn(el);*/
    yeah* bb = evl_fully(b, env);
    //dumpn(bb);
    if (equal(bb, True)) {
      return freeze(th, env);
    } else if (equal(bb, False)) {
      return freeze(el, env);
    } else {
      spew(("Not a bool: "));
      dumpn(bb);
      err((""));
    }
  } else if (e->t == APP) {
    return evl_step(app(evl_step(e->u.app.f,env), freeze(e->u.app.arg, env)), env);
  } else if (e->t == THUNK) {
    return evl_step(e->u.thunk.exp, e->u.thunk.env);
  } else if (e->t == LAMBDA) {
    return closure(e, env);
  } else if (e->t == SYMBOL) {
    return lookup(e->u.symbol.s, env);
  } else if (e->t == CSYMBOL) {
    return symbol(e->u.csymbol.s);
  } else if (e->t == PAIR) {
    return pair(thunk(e->u.pair.car, env), thunk(e->u.pair.cdr, env));
  } else if (e->t == INTEGER || e->t == CLOSURE) {
    return e;
  } else {
    warn(("Can't eval "));
    dumpn(e);
    err((""));
  }
}

yeah* evl_step(yeah* e, yeah* env) {
  static int indent = 0;

  if (!trace || indent > max_trace_show) {
    yeah* value = evl_step_(e, env);
#if 0
if (value->t == SYMBOL && !strcmp(value->u.symbol.s, "x")) {
  dumpn(e);
  dumpn(env);
  err((""));
}
#endif
 return value;
  }

  for (int i = 0; i < indent; ++i) {
    printf("| ");
  }
  printf("+ ");
  dump(e);

  if (trace_env_too) {
    printf("  [");
    dump(env);
    printf("]");
  }
  printf("\n");

  indent++;
  yeah* value = evl_step_(e, env);
  indent--;

  for (int i = 0; i < indent; ++i) {
    printf("| ");
  }
  printf("=> ");
  dumpn(value);

  return value;
}

bool is_data(yeah* e) {
  int t = e->t;
  return t == INTEGER | t == SYMBOL || t == PAIR;
}

yeah* evl_fully(yeah* e, yeah* env) {
  yeah* ee = evl_step(e, env);
  if (is_data(ee) || equal(e, ee)) {
    return ee;
  } else {
    return evl_fully(ee, env);
  }
}

void topevl(char* src, yeah* obj) {
  printf("+ %s\n", src);
  yeah* value = evl_fully(obj, symbol("Nil"));
  printf("=> ");
  dumpn(value);
}

void init() {
  Nil = symbol("Nil");
  True = symbol("True");
  False = symbol("False");
  globals = symbol("Nil");
}

int main(int argc, char** argv) {
  init();
  //trace = 1;
/*   topevl(lambda(symbol("x"), symbol("x"))); */
/*   topevl(app(lambda(symbol("x"), symbol("x")), integer(1))); */
/*   topevl(integer(10)); */
/*   topevl(app(app(symbol("+"), integer(1)), integer(2))); */

#include "obj.i"

  //printf("steps %d\n", count);

  return 0;
}
