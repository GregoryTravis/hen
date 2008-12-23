#include <stdio.h>
#include <string.h>

#include "a.h"
#include "mem.h"
#include "spew.h"

void fbo_main();


static int trace = 0;
static int count_reductions = 0;
static int show_bindings = 0;
static int trace_env_too = 0;
static int max_trace_show = 6;
static int pretty = 1;

static int n_reductions = 0;
void count_reductions_start() {
  n_reductions = 0;
}
void count_reductions_end() {
  if (count_reductions) {
    printf("%d reductions.\n");
  }
}

typedef enum {
  LAMBDA,
  CLOSURE,
  THUNK,
  INTEGER,
  SYMBOL,
  PAIR,
  APP,
  CSYMBOL,
  OPAQUE,
} tag;

typedef struct yeah yeah;

yeah *Nil, *True, *False, *CNil;
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
      void* q;
    } opaque;

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

yeah* opaque(void* q) {
  yeah* y = newyeah();
  y->t = OPAQUE;
  y->u.opaque.q = q;
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

bool nilp(yeah* e) {
  return (e->t == CSYMBOL || e->t == SYMBOL) && !strcmp(e->u.csymbol.s, "Nil");
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
  case OPAQUE: err(("Comparison of opaque 0x%x\n", a->u.opaque.q));
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

#define ISTAG(_e, _t) ((_e)->t == (_t))
#define ISLAMBDA(_e) ISTAG((_e), LAMBDA)
#define ISCLOSURE(_e) ISTAG((_e), CLOSURE)
#define ISTHUNK(_e) ISTAG((_e), THUNK)
#define ISINTEGER(_e) ISTAG((_e), INTEGER)
#define ISSYMBOL(_e) ISTAG((_e), SYMBOL)
#define ISPAIR(_e) ISTAG((_e), PAIR)
#define ISAPP(_e) ISTAG((_e), APP)
#define ISCSYMBOL(_e) ISTAG((_e), CSYMBOL)
#define ISOPAQUE(_e) ISTAG((_e), OPAQUE)

char* symstring(yeah* e) {
  A(ISSYMBOL(e));
  return e->u.symbol.s;
}

char* csymstring(yeah* e) {
  A(ISCSYMBOL(e));
  return e->u.symbol.s;
}

void* opaqueval(yeah* e) {
  A(ISOPAQUE(e));
  return e->u.opaque.q;
}

bool isthissymbol(yeah* e, char* s) {
  return ISSYMBOL(e) && !strcmp(symstring(e), s);
}

bool isthiscsymbol(yeah* e, char* s) {
  return ISCSYMBOL(e) && !strcmp(csymstring(e), s);
}

yeah* car(yeah* e) {
  A(ISPAIR(e));
  return e->u.pair.car;
}

yeah* cdr(yeah* e) {
  A(ISPAIR(e));
  return e->u.pair.cdr;
}

int getint(yeah* e) {
  A(ISINTEGER(e));
  return e->u.integer.i;
}

yeah* appfun(yeah* app) {
  A(ISAPP(app));
  return app->u.app.f;
}

yeah* apparg(yeah* app) {
  A(ISAPP(app));
  return app->u.app.arg;
}

yeah* closurelam(yeah* closure) {
  A(ISCLOSURE(closure));
  return closure->u.closure.lambda;
}

yeah* closureenv(yeah* closure) {
  A(ISCLOSURE(closure));
  return closure->u.closure.env;
}

yeah* lambdaarg(yeah* lambda) {
  A(ISLAMBDA(lambda));
  return lambda->u.lambda.arg;
}

yeah* lambdabody(yeah* lambda) {
  A(ISLAMBDA(lambda));
  return lambda->u.lambda.body;
}

void* opaque_val(yeah* y) {
  A(ISOPAQUE(y));
  return y->u.opaque.q;
}

void* opaque_set(yeah* y, void* v) {
  A(ISOPAQUE(y));
  y->u.opaque.q = v;
}

#define APPF(e) ((e)->u.app.f)
#define apparg(e) ((e)->u.app.arg)

#define TF(_b) ((_b) ? True : False)

bool is_ctor(yeah* y) {
  if (ISSYMBOL(y)) {
    char c = symstring(y)[0];
    return c >= 'A' &&c <= 'Z';
  } else {
    return false;
  }
}

bool is_cton(yeah* y) {
  return ISPAIR(y) && is_ctor(car(y));
}

bool is_high_cons(yeah* y) {
  return ISPAIR(y) && isthissymbol(car(y), "Cons") && ISPAIR(cdr(y)) && ISPAIR(cdr(cdr(y))) && nilp(cdr(cdr(cdr(y))));
}

yeah* hcar(yeah* y) {
  A(is_high_cons(y));
  return car(cdr(y));
}

yeah* hcdr(yeah* y) {
  A(is_high_cons(y));
  return car(cdr(cdr(y)));
}

yeah* hcadr(yeah* y) { return hcar(hcdr(y)); }

void dump(yeah* y);

void dump_list(yeah* y) {
  printf("($");
  yeah* here = y;
  while (is_high_cons(here)) {
    printf(" ");
    dump(hcar(here));
    here = hcdr(here);
  }
  if (!nilp(here)) {
    printf(" . ");
    dump(here);
  }
  printf(")");
}

void dump_cton(yeah* y) {
  if (is_high_cons(y)) {
    dump_list(y);
    return;
  }

  printf("(");

  yeah* here = y;
  while (!nilp(here)) {
    dump(car(here));
    here = cdr(here);
    if (!nilp(here)) {
      printf(" ");
    }
  }

  printf(")");
}

void dump(yeah* y) {
  switch (y->t) {
  case INTEGER: printf( "%d", y->u.integer.i ); break;
  case SYMBOL: printf( "%s", y->u.symbol.s ); break;
  case CSYMBOL: printf( "'%s", y->u.symbol.s ); break;
  case OPAQUE: printf("(Q 0x%x)", y->u.opaque.q); break;
  case PAIR:
    if (pretty && is_cton(y)) {
      dump_cton(y);
    } else {
      printf( "(P " );
      dump(y->u.pair.car);
      printf( " " );
      dump(y->u.pair.cdr);
      printf( ")" );
    }
    break;
  case THUNK:
    printf( "(@ ");
    dump(y->u.thunk.exp);
    printf(")");
    dump(y->u.thunk.env);
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
  default: err(("%d\n", y->t)); break;
  }
}

void dumpn(yeah* y) {
  dump(y);
  putchar('\n');
}

bool isprim1(yeah* e, char* s, yeah** arg0) {
  if (ISAPP(e) && isthissymbol(appfun(e), s)) {
    *arg0 = apparg(e);
    return true;
  } else {
    return false;
  }
}

bool isprim2(yeah* e, char* s, yeah** arg0, yeah** arg1) {
  if (ISAPP(e) && isprim1(appfun(e), s, arg0)) {
    *arg1 = apparg(e);
    return true;
  } else {
    return false;
  }
}

bool isprim3(yeah* e, char* s, yeah** arg0, yeah** arg1, yeah** arg2) {
  if (ISAPP(e) && isprim2(appfun(e), s, arg0, arg1)) {
    *arg2 = apparg(e);
    return true;
  } else {
    return false;
  }
}

yeah* evl_step(yeah* e, yeah* env);
yeah* evl_fully(yeah* e, yeah* env);
yeah* evl_completely(yeah* e, yeah* env);

yeah* evl_step_(yeah* e, yeah* env) {
  yeah *arg0, *arg1, *arg2;
  if (isprim1(e, "PAIR?", &arg0)) {
    return TF(ISPAIR(evl_fully(arg0, env)));
  } else if (isprim1(e, "CAR", &arg0)) {
    return car(evl_fully(arg0, env));
  } else if (isprim1(e, "CDR", &arg0)) {
    return cdr(evl_fully(arg0, env));
  } else if (ISAPP(e) && ISSYMBOL(appfun(e))) {
    return app(lookup(symstring(appfun(e)), env), freeze(apparg(e), env));
  } else if (isprim2(e, "+", &arg0, &arg1)) {
    return integer(getint(evl_completely(arg0, env)) + getint(evl_completely(arg1, env)));
  } else if (isprim2(e, "-", &arg0, &arg1)) {
    return integer(getint(evl_completely(arg0, env)) - getint(evl_completely(arg1, env)));
  } else if (isprim2(e, "*", &arg0, &arg1)) {
    return integer(getint(evl_completely(arg0, env)) * getint(evl_completely(arg1, env)));
  } else if (isprim2(e, "cons", &arg0, &arg1)) {
    return pair(freeze(arg0, env), freeze(arg1, env));
  } else if (isprim2(e, "==", &arg0, &arg1)) {
    return TF(equal(evl_fully(arg0, env), evl_fully(arg1, env)));
  } else if (ISAPP(e) && ISAPP(appfun(e)) && ISSYMBOL(appfun(appfun(e)))) {
    return app(app(lookup(symstring(appfun(appfun(e))), env), freeze(apparg(appfun(e)), env)), freeze(apparg(e), env));
  } else if (e->t == APP && e->u.app.f->t == CLOSURE) {
    if (show_bindings) { printf("BIND %s\n", e->u.app.f->u.closure.lambda->u.lambda.arg->u.symbol.s); dumpn(e->u.app.arg); }
    return freeze(lambdabody(closurelam(appfun(e))),
      pair(pair(lambdaarg(closurelam(appfun(e))), apparg(e)), closureenv(appfun(e))));
  } else if (isprim3(e, "if", &arg0, &arg1, &arg2)) {
    arg0 = evl_completely(arg0, env);
    if (equal(arg0, True)) {
      return freeze(arg1, env);
    } else if (equal(arg0, False)) {
      return freeze(arg2, env);
    } else {
      spew(("Not a bool: "));
      dumpn(arg0);
      err((""));
    }
  } else if (ISAPP(e)) {
    return app(evl_step(e->u.app.f,env), freeze(e->u.app.arg, env));
  } else if (ISTHUNK(e)) {
    return evl_step(e->u.thunk.exp, e->u.thunk.env);
  } else if (ISLAMBDA(e)) {
    return closure(e, env);
  } else if (ISSYMBOL(e)) {
    return lookup(e->u.symbol.s, env);
  } else if (ISCSYMBOL(e)) {
    return symbol(e->u.csymbol.s);
  } else if (ISPAIR(e)) {
    return pair(thunk(e->u.pair.car, env), thunk(e->u.pair.cdr, env));
  } else if (ISINTEGER(e) || ISCLOSURE(e) || ISOPAQUE(e)) {
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
  printf("-> ");
  dumpn(value);

  return value;
}

bool is_data(yeah* e) {
  int t = e->t;
  return t == INTEGER | t == SYMBOL || t == PAIR || t == OPAQUE || t == CLOSURE;
}

yeah* evl_fully(yeah* e, yeah* env) {
  yeah* ee = evl_step(e, env);
  if (is_data(ee) || equal(e, ee)) {
    return ee;
  } else {
    return evl_fully(ee, env);
  }
}

yeah* evl_completely(yeah* e, yeah* env) {
  yeah* ee = evl_fully(e, env);
  if (ISPAIR(ee)) {
    return pair(evl_completely(car(ee), env), evl_completely(cdr(ee), env));
  } else {
    return ee;
  }
}

yeah* evl(yeah* e) {
  return evl_completely(e, Nil);
}

yeah* create_ref(yeah* arg) {
  yeah* r = opaque(NEW(int));
  opaque_set(r, (void*)getint(arg));
  return r;
}

yeah* read_ref(yeah* arg) {
  A(opaque_val(arg) != NULL);
  return integer((int)opaque_val(arg));
}

yeah* write_ref(yeah* arg) {
  yeah* box = hcar(arg);
  A(opaque_val(box) != NULL);
  yeah* value = hcadr(arg);
  opaque_set(box, (void*)getint(value));
  return Nil;
}

yeah* destroy_ref(yeah* arg) {
  A(opaque_val(arg) != NULL);
  fri(opaque_val(arg));
  opaque_set(arg, (void*)NULL);
}

yeah* execute_command(yeah* name, yeah* arg) {
  printf("Command: %s ", symstring(name));
  dumpn(arg);

  A(ISSYMBOL(name));
  char* ns = symstring(name);
  if (!strcmp(ns, "shew")) {
    printf("(SHEW ");
    dump(arg);
    printf(")\n");
    return CNil;
  } else if (!strcmp(ns, "create-ref")) {
    return create_ref(arg);
  } else if (!strcmp(ns, "read-ref")) {
    return read_ref(arg);
  } else if (!strcmp(ns, "write-ref")) {
    return write_ref(arg);
  } else if (!strcmp(ns, "destroy-ref")) {
    return destroy_ref(arg);
  } else if (!strcmp(ns, "fbo")) {
    fbo_main();
  } else {
    dumpn(name);
    dumpn(arg);
    err(("Unknown command!"));
  }
}

bool iscommand(yeah* e, yeah** name, yeah** arg, yeah** k) {
  if (!ISPAIR(e)) return false;
  if (!isthissymbol(car(e), "X")) return false;
  yeah* d = cdr(e);
  if (!ISPAIR(d)) return false;
  *name = car(d);
  if (!ISSYMBOL(*name)) return false;
  yeah* dd = cdr(d);
  if (!ISPAIR(dd)) return false;
  yeah* ddd = cdr(dd);
  if (!ISPAIR(ddd)) return false;
  *arg = car(dd);
  if (!nilp(cdr(ddd))) {
    dump(e);
    err(("Bad command!"));
  }
  *k = car(ddd);
  return true;
}

yeah* evl_driver(yeah* e) {
  yeah *name, *arg, *k;
  yeah* ee = evl(e);
  if (iscommand(ee, &name, &arg, &k)) {
    yeah* output = execute_command(name, arg);
    return evl_driver(app(k, pair(output, Nil)));
  } else {
    return evl(e);
  }
}

void evl_top(char* src, yeah* e) {
  printf("+ %s\n", src);
  yeah* value = evl_driver(e);
  printf("=> ");
  dumpn(value);
}

void init_constants() {
  Nil = symbol("Nil");
  True = symbol("True");
  False = symbol("False");
  CNil = csymbol("Nil");
  globals = symbol("Nil");
}

int main(int argc, char** argv) {
  init_constants();

  count_reductions_start();
#include "obj.i"
  count_reductions_end();

  return 0;
}
