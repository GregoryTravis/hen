#include <stdio.h>
#include <stdlib.h>

#define ASSERT(x) do { if (!(x)) { fprintf(stderr, "ASSERTION FAILURE\n"); exit(1); } } while (0)

typedef enum {
  LAMBDA,
  CLOSURE,
  THUNK,
  INTEGER,
  SYMBOL,
  PAIR,
  APP,
} tag;

typedef struct yeah yeah;

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
  } u;
};

yeah* newyeah(void) {
  yeah* y = malloc(sizeof(yeah));
  ASSERT(y);
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

void dump1(yeah* y) {
  switch (y->t) {
  case INTEGER: printf( "%d", y->u.integer.i ); break;
  case SYMBOL: printf( "%s", y->u.symbol.s ); break;
  case PAIR:
    printf( "(" );
    dump1(y->u.pair.car);
    printf( " . " );
    dump1(y->u.pair.cdr);
    printf( ")" );
    break;
  case THUNK:
    printf( "(@ ");
    dump1(y->u.thunk.exp);
    printf( " ");
    dump1(y->u.thunk.env);
    printf( ")");
    break;
  case CLOSURE:
    printf( "($ ");
    dump1(y->u.closure.lambda);
    printf( " ");
    dump1(y->u.closure.env);
    printf( ")");
    break;
  case LAMBDA:
    printf( "(/. ");
    dump1(y->u.lambda.arg);
    printf( " ");
    dump1(y->u.lambda.body);
    printf( ")");
    break;
  case APP:
    printf( "(");
    dump1(y->u.app.f);
    printf( " ");
    dump1(y->u.app.arg);
    printf( ")");
    break;
  default: ASSERT(0); break;
  }
}

void dump(yeah* y) {
  dump1(y);
  putchar('\n');
}

int main(int argc, char** argv) {
  yeah* y = lambda(symbol("x"), symbol("x"));
  dump(y);
  return 0;
}
