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
    ASSERT(env->t == PAIR);
    ASSERT(env->u.pair.car->t == PAIR);
    ASSERT(env->u.pair.car->u.pair.car->t == SYMBOL);
    if (!strcmp(s, env->u.pair.car->u.pair.car->u.symbol.s)) {
      return env->u.pair.car->u.pair.cdr;
    } else {
      return lookup(s, env->u.pair.cdr);
    }
  }
}

yeah* evl(yeah* e, yeah* env);

yeah* evl_(yeah* e, yeah* env) {
  if (e->t == APP && e->u.app.f->t == APP && e->u.app.f->u.app.f->t == SYMBOL) {
    char* f = e->u.app.f->u.app.f->u.symbol.s;
    yeah* a = e->u.app.f->u.app.arg;
    yeah* b = e->u.app.arg;
    if (!strcmp(f, "+")) {
      ASSERT(a->t == INTEGER && b->t == INTEGER);
      return integer(a->u.integer.i + b->u.integer.i);
    } else {
      err(("Unknown primitive %s\n", f));
    }
  } else if (e->t == APP && e->u.app.f->t == CLOSURE) {
    return evl(
      e->u.app.f->u.closure.lambda->u.lambda.body,
      pair(
        pair(
          e->u.app.f->u.closure.lambda->u.lambda.arg,
          e->u.app.arg),
        e->u.app.f->u.closure.env));
  } else if (e->t == APP) {
    return evl(app(evl(e->u.app.f,env), e->u.app.arg), e->u.app.arg);
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

yeah* evl(yeah* e, yeah* env) {
  if (!trace) {
    return evl_(e, env);
  }

  static int indent = 0;
  indent++;

  for (int i = 0; i < indent; ++i) {
    printf("| ");
  }
  printf("+ ");
  dump(e);
  printf("  [");
  dump(env);
  printf("]");
  printf("\n");

  yeah* value = evl_(e, env);

  for (int i = 0; i < indent; ++i) {
    printf("| ");
  }
  printf("-> ");
  dumpn(value);

  indent--;

  return value;
}


void topevl(yeah* y) {
  printf("+ ");
  dumpn(y);
  yeah* value = evl(y,Nil);
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
