#include <stdio.h>
#include <string.h>

#include "vor.h"

#include "a.h"
#include "mem.h"
#include "spew.h"

// HEY
#include "ref.impl.h"
#include "shew.impl.h"

void hen_main();
yeah* evl_fully(yeah* e, yeah* env);
yeah* evl_completely(yeah* e, yeah* env);

static int trace = 0;
static int count_reductions = 0;
static int show_bindings = 0;
static int trace_env_too = 0;
static int max_trace_show = 6;
static int max_dump_level = 6;
static int pretty = 1;
static int show_commands = 0;

static int n_reductions = 0;
void count_reductions_start() {
  n_reductions = 0;
}
void count_reductions_end() {
  if (count_reductions) {
    printf("%d reductions.\n");
  }
}

yeah *Nil, *True, *False, *CNil;
yeah* globals;

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

yeah* string(char* s) {
  yeah* y = newyeah();
  y->t = STRING;
  y->u.string.s = s;
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

yeah* flote(float f) {
  yeah* y = newyeah();
  y->t = FLOAT;
  y->u.flote.f = f;
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

bool isnil(yeah* e) {
  return (e->t == CSYMBOL || e->t == SYMBOL) && !strcmp(e->u.csymbol.s, "Nil");
}

yeah* store_global(char *s, yeah* v) {
  globals = pair(pair(symbol(s), v), globals);
}

yeah* lookup_in_env(char* s, yeah* env) {
  if (isnil(env)) {
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
  case FLOAT: return a->u.flote.f == b->u.flote.f;
    break;
  case SYMBOL: return !strcmp(a->u.symbol.s, b->u.symbol.s);
    break;
  case CSYMBOL: return !strcmp(a->u.csymbol.s, b->u.csymbol.s);
    break;
  case OPAQUE: err(("Comparison of opaque 0x%x\n", a->u.opaque.q));
    break;
  case LAMBDA: case CLOSURE: case THUNK: case PAIR: case APP:
    return
      equal(a->u.generic_pointer_pair.a, b->u.generic_pointer_pair.a) &&
      equal(a->u.generic_pointer_pair.b, b->u.generic_pointer_pair.b);
    break;
  case STRING:
    return !strcmp(a->u.string.s, b->u.string.s);
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
#define ISFLOAT(_e) ISTAG((_e), FLOAT)
#define ISSYMBOL(_e) ISTAG((_e), SYMBOL)
#define ISPAIR(_e) ISTAG((_e), PAIR)
#define ISAPP(_e) ISTAG((_e), APP)
#define ISCSYMBOL(_e) ISTAG((_e), CSYMBOL)
#define ISSTRING(_e) ISTAG((_e), STRING)
#define ISOPAQUE(_e) ISTAG((_e), OPAQUE)

char* symstring(yeah* e) {
  A(ISSYMBOL(e));
  return e->u.symbol.s;
}

char* csymstring(yeah* e) {
  A(ISCSYMBOL(e));
  return e->u.symbol.s;
}

char* stringval(yeah* e) {
  A(ISSTRING(e));
  return e->u.string.s;
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

float getfloat(yeah* e) {
  A(ISFLOAT(e));
  return e->u.flote.f;
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
  return ISPAIR(y) && isthissymbol(car(y), "Cons") && ISPAIR(cdr(y)) && ISPAIR(cdr(cdr(y))) && isnil(cdr(cdr(cdr(y))));
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

void dump_list(yeah* y) {
  printf("($");
  yeah* here = y;
  while (is_high_cons(here)) {
    printf(" ");
    dump(hcar(here));
    here = hcdr(here);
  }
  if (!isnil(here)) {
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
  while (!isnil(here)) {
    dump(car(here));
    here = cdr(here);
    if (!isnil(here)) {
      printf(" ");
    }
  }

  printf(")");
}

int dump_level = 0;

void dump(yeah* y) {
  if (dump_level > max_dump_level) {
    printf("...");
    return;
  }

  dump_level++;
  switch (y->t) {
  case INTEGER: printf("%d", y->u.integer.i); break;
  case FLOAT: printf("%f", y->u.flote.f); break;
  case SYMBOL: printf("%s", y->u.symbol.s); break;
  case CSYMBOL: printf("'%s", y->u.symbol.s); break;
  case STRING: printf("\"%s\"", y->u.string.s); break;
  case OPAQUE: printf("(Q)"); break;
  case PAIR:
    if (pretty && is_cton(y)) {
      dump_cton(y);
    } else {
      printf("(P ");
      dump(y->u.pair.car);
      printf(" ");
      dump(y->u.pair.cdr);
      printf(")");
    }
    break;
  case THUNK:
    printf("(@ ");
    dump(y->u.thunk.exp);
    printf(")");
    dump(y->u.thunk.env);
    break;
  case CLOSURE:
    printf("($ ");
    dump(y->u.closure.lambda);
    printf(" ");
    dump(y->u.closure.env);
    printf(")");
    break;
  case LAMBDA:
    printf("(/. ");
    dump(y->u.lambda.arg);
    printf(" ");
    dump(y->u.lambda.body);
    printf(")");
    break;
  case APP:
    printf("(");
    dump(y->u.app.f);
    printf(" ");
    dump(y->u.app.arg);
    printf(")");
    break;
  default: err(("%d (ptr %d)\n", y->t, y)); break;
  }
  dump_level--;
}

void dumpn(yeah* y) {
  dump(y);
  putchar('\n');
}

void tdumpn(char* s, yeah* y) {
  printf("%s ", s);
  dumpn(y);
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

bool try_prim(yeah* e, yeah* env, yeah** result) {
  yeah *arg0, *arg1, *arg2;
  if (isprim1(e, "PAIR?", &arg0)) {
    *result = TF(ISPAIR(evl_fully(arg0, env)));
    return true;
  } else if (isprim1(e, "CAR", &arg0)) {
    *result = car(evl_fully(arg0, env));
    return true;
  } else if (isprim1(e, "CDR", &arg0)) {
    *result = cdr(evl_fully(arg0, env));
    return true;
  } else if (ISAPP(e) && ISSYMBOL(appfun(e))) {
    *result = app(lookup(symstring(appfun(e)), env), freeze(apparg(e), env));
    return true;
  } else if (isprim2(e, "+", &arg0, &arg1)) {
    *result = integer(getint(evl_completely(arg0, env)) + getint(evl_completely(arg1, env)));
    return true;
  } else if (isprim2(e, "-", &arg0, &arg1)) {
    *result = integer(getint(evl_completely(arg0, env)) - getint(evl_completely(arg1, env)));
    return true;
  } else if (isprim2(e, "*", &arg0, &arg1)) {
    *result = integer(getint(evl_completely(arg0, env)) * getint(evl_completely(arg1, env)));
    return true;
  } else if (isprim2(e, "cons", &arg0, &arg1)) {
    *result = pair(freeze(arg0, env), freeze(arg1, env));
    return true;
  } else if (isprim2(e, "==", &arg0, &arg1)) {
    *result = TF(equal(evl_fully(arg0, env), evl_fully(arg1, env)));
    return true;
  } else if (ISAPP(e) && ISAPP(appfun(e)) && ISSYMBOL(appfun(appfun(e)))) {
    *result = app(app(lookup(symstring(appfun(appfun(e))), env), freeze(apparg(appfun(e)), env)), freeze(apparg(e), env));
    return true;
  } else if (e->t == APP && e->u.app.f->t == CLOSURE) {
    if (show_bindings) { printf("BIND %s\n", e->u.app.f->u.closure.lambda->u.lambda.arg->u.symbol.s); dumpn(e->u.app.arg); }
    *result = freeze(lambdabody(closurelam(appfun(e))),
      pair(pair(lambdaarg(closurelam(appfun(e))), apparg(e)), closureenv(appfun(e))));
    return true;
  } else if (isprim3(e, "if", &arg0, &arg1, &arg2)) {
    arg0 = evl_completely(arg0, env);
    if (equal(arg0, True)) {
      *result = freeze(arg1, env);
      return true;
    } else if (equal(arg0, False)) {
      *result = freeze(arg2, env);
      return true;
    } else {
      spew(("Not a bool: "));
      dumpn(arg0);
      err((""));
    }
  } else {
    return false;
  }
}

yeah* evl_step(yeah* e, yeah* env);
yeah* evl_fully(yeah* e, yeah* env);
yeah* evl_completely(yeah* e, yeah* env);

yeah* evl_step_(yeah* e, yeah* env) {
  if (ISAPP(e)) {
    yeah* result;
    if (try_prim(e, env, &result)) {
      return result;
    } else {
      return app(evl_step(e->u.app.f,env), freeze(e->u.app.arg, env));
    }
  } else if (ISTHUNK(e)) {
    return evl_step(e->u.thunk.exp, e->u.thunk.env);
  } else if (ISLAMBDA(e)) {
    return closure(e, env);
  } else if (is_ctor(e)) {
    return e;
  } else if (ISSYMBOL(e)) {
    return lookup(e->u.symbol.s, env);
  } else if (ISCSYMBOL(e)) {
    return symbol(e->u.csymbol.s);
  } else if (ISPAIR(e)) {
    return pair(thunk(e->u.pair.car, env), thunk(e->u.pair.cdr, env));
  } else if (ISINTEGER(e) || ISFLOAT(e) || ISCLOSURE(e) || ISOPAQUE(e) || ISSTRING(e)) {
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
    return evl_step_(e, env);
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
  return t == INTEGER || t == FLOAT || t == SYMBOL || t == PAIR || t == OPAQUE || t == CLOSURE;
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

yeah* foreign_functions;
void register_command(char *name, foreign_function f) {
  foreign_functions = pair(pair(symbol(name), opaque(f)), foreign_functions);
}

yeah* execute_command(yeah* name, yeah* arg) {
  if (show_commands) {
    printf("Command: %s ", symstring(name));
    dumpn(arg);
  }

  A(ISSYMBOL(name));
  yeah* v = lookup_in_env(symstring(name), foreign_functions);

  if (v == NULL) {
    dumpn(name);
    dumpn(arg);
    err(("Unknown command!"));
  }

  foreign_function f = (foreign_function)opaqueval(v);
  yeah* ret = (*f)(arg);
  if (show_commands) {
    printf("Command %s returns ", symstring(name));
    dumpn(ret);
  }
  return ret;
}

yeah* evl_driver(yeah* e) {
  //printf("evl_driver: "); dumpn(e);
  yeah *name, *args, *k;
  yeah* ee = e; // evl(e);

  if (ISPAIR(ee) && isthissymbol(car(ee), "CommandSeq")) {
    yeah* command = car(cdr(ee));
    yeah* k = car(cdr(cdr(ee)));

/*
    A(isthissymbol(car(command), "Command"));
    yeah* name = car(cdr(command));
    yeah* args = car(cdr(cdr(command)));
    yeah* output = execute_command(name, args);
*/
    yeah* output = evl_driver(command);
    return evl_driver(evl(app(k, pair(output, Nil))));
  } else if (ISPAIR(ee) && isthissymbol(car(ee), "Command")) {
    yeah* command = ee;
    A(isthissymbol(car(command), "Command"));
    yeah* name = car(cdr(command));
    yeah* args = car(cdr(cdr(command)));
    yeah* output = execute_command(name, args);
    return output;
  } else if (ISPAIR(ee) && isthissymbol(car(ee), "Return")) {
    return car(cdr(ee));
//  } else if (isthissymbol(ee, "EndCommand")) {
//    return Nil;
  } else {
    // HEY um, just return ee?
    //return evl(e);
    return ee;
  }
}

void evl_from_callback(yeah* e) {
  yeah* r = evl_driver(evl(e));
  //printf("evl_from_callback returns "); dumpn(r);
}

void evl_top(char* src, yeah* e) {
  printf("+ %s\n", src);
  yeah* value = evl_driver(evl(e));
  printf("=> ");
  dumpn(value);
}

void init_constants() {
  Nil = symbol("Nil");
  True = symbol("True");
  False = symbol("False");
  CNil = csymbol("Nil");
  globals = symbol("Nil");
  foreign_functions = symbol("Nil");

  // HEY rid
  ref_impl_register();
  shew_impl_register();
}

int main(int argc, char** argv) {
  init_constants();

  count_reductions_start();
  hen_main();
  count_reductions_end();

  return 0;
}

static void invoke_hen_fun_wrapper(yeah* wfun) {
  evl_from_callback(app(wfun, Nil));
}

/*
static yeah* wrapped_hen_fun;
static void hen_fun_wrapper(void) {
  invoke_hen_fun_wrapper(wrapped_hen_fun);
}
*/

#define DECLARE_WRAPPER_FUN(n) static void hen_fun_wrapper_##n(void) { invoke_hen_fun_wrapper(wrapped_hen_funs[(n)]); }

#define NCALLBACKS 4
static yeah *wrapped_hen_funs[NCALLBACKS];

DECLARE_WRAPPER_FUN(0)
DECLARE_WRAPPER_FUN(1)
DECLARE_WRAPPER_FUN(2)
DECLARE_WRAPPER_FUN(3)

static vvfunp goobie[NCALLBACKS] = {
  &hen_fun_wrapper_0,
  &hen_fun_wrapper_1,
  &hen_fun_wrapper_2,
  &hen_fun_wrapper_3,
};

static int cb_cursor = 0;

vvfunp wrap_hen_fun(yeah* f) {
  A(cb_cursor < NCALLBACKS);
  int cbi = cb_cursor++;
  //printf("WRAP %d!!!\n", cbi);
  wrapped_hen_funs[cbi] = f;
  return goobie[cbi];
}
