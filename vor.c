#include <stdio.h>
#include <string.h>

#include "a.h"
#include "mem.h"
#include "spew.h"

static int trace = 0;

typedef enum {
  LAMBDA,
  CLOSURE,
  THUNK,
  INTEGER,
  SYMBOL,
  PAIR,
  APP,
  NIL,
} tag;

typedef struct yeah yeah;

yeah *Nil;

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
    } nil;

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
  yeah* y = newyeah();
  y->t = NIL;
  return y;
}

void dump(yeah* y) {
  switch (y->t) {
  case INTEGER: printf( "%d", y->u.integer.i ); break;
  case SYMBOL: printf( "%s", y->u.symbol.s ); break;
  case PAIR:
    printf( "(" );
    dump(y->u.pair.car);
    printf( " . " );
    dump(y->u.pair.cdr);
    printf( ")" );
    break;
  case THUNK:
    printf( "(@ ");
    dump(y->u.thunk.exp);
    printf( " ");
    dump(y->u.thunk.env);
    printf( ")");
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
  case NIL:
    printf( "Nil");
    break;
  default: err(("%d\n", y->t)); break;
  }
}

void dumpn(yeah* y) {
  dump(y);
  putchar('\n');
}

yeah* lookup(char* s, yeah* env) {
  if (env == Nil) {
    err(("No such variable %s\n", s ));
  } else {
    A(env->t == PAIR);
    A(env->u.pair.car->t == PAIR);
    A(env->u.pair.car->u.pair.car->t == SYMBOL);
    if (!strcmp(s, env->u.pair.car->u.pair.car->u.symbol.s)) {
      return env->u.pair.car->u.pair.cdr;
    } else {
      return lookup(s, env->u.pair.cdr);
    }
  }
}

yeah* freeze(yeah* e, yeah* env) {
  if (e->t == THUNK) {
    return e;
  } else {
    return thunk(e, env);
  }
}

yeah* evl_step(yeah* e, yeah* env);

yeah* evl_step_(yeah* e, yeah* env) {
  if (e->t == APP && e->u.app.f->t == APP && e->u.app.f->u.app.f->t == SYMBOL) {
    char* f = e->u.app.f->u.app.f->u.symbol.s;
    yeah* a = e->u.app.f->u.app.arg;
    yeah* b = e->u.app.arg;
    if (!strcmp(f, "+")) {
      A(a->t == INTEGER && b->t == INTEGER);
      return integer(a->u.integer.i + b->u.integer.i);
    } else {
      err(("Unknown primitive %s\n", f));
    }
  } else if (e->t == APP && e->u.app.f->t == CLOSURE) {
    return evl_step(
      e->u.app.f->u.closure.lambda->u.lambda.body,
      pair(
        pair(
          e->u.app.f->u.closure.lambda->u.lambda.arg,
          e->u.app.arg),
        e->u.app.f->u.closure.env));
  } else if (e->t == APP) {
    return evl_step(app(evl_step(e->u.app.f,env), freeze(e->u.app.arg, env)), env);
  } else if (e->t == THUNK) {
    return evl_step(e->u.thunk.exp, e->u.thunk.env);
  } else if (e->t == LAMBDA) {
    return closure(e, env);
  } else if (e->t == SYMBOL) {
    return lookup(e->u.symbol.s, env);
  } else if (e->t == INTEGER || e->t == CLOSURE || e->t == NIL) {
    return e;
  } else {
    warn(("Can't eval "));
    dumpn(e);
    err((""));
  }
}

yeah* evl_step(yeah* e, yeah* env) {
  if (!trace) {
    return evl_step_(e, env);
  }

  static int indent = 0;

  for (int i = 0; i < indent; ++i) {
    printf("| ");
  }
  printf("+ ");
  dump(e);
  printf("  [");
  dump(env);
  printf("]");
  printf("\n");

  indent++;
  yeah* value = evl_step_(e, env);
  indent--;

  for (int i = 0; i < indent; ++i) {
    printf("| ");
  }
  printf("-> ");
  dumpn(value);

  return value;
}

bool is_data(yeah* e) {
  int t = e->t;
  return t == INTEGER | t == SYMBOL || t == PAIR;
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

yeah* evl_fully(yeah* e, yeah* env) {
  yeah* ee = evl_step(e, env);
  if (is_data(ee) || equal(e, ee)) {
    return ee;
  } else {
    return evl_fully(ee, env);
  }
}

void topevl(yeah* y) {
  printf("+ ");
  dumpn(y);
  yeah* value = evl_fully(y, Nil);
  printf("-> ");
  dumpn(value);
}

void init() {
  Nil = nil();
}

int main(int argc, char** argv) {
  init();
  //trace = 1;
/*   topevl(lambda(symbol("x"), symbol("x"))); */
/*   topevl(app(lambda(symbol("x"), symbol("x")), integer(1))); */
/*   topevl(integer(10)); */
/*   topevl(app(app(symbol("+"), integer(1)), integer(2))); */

#include "obj.i"

  return 0;
}
