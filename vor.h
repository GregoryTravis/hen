#include "a.h"

typedef enum {
  LAMBDA,
  CLOSURE,
  THUNK,
  INTEGER,
  SYMBOL,
  PAIR,
  APP,
  CSYMBOL,
  STRING,
  OPAQUE,
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

    struct {
      char* s;
    } csymbol;

    struct {
      char* s;
    } string;

    struct {
      void* q;
    } opaque;

    struct {
      yeah* a;
      yeah* b;
    } generic_pointer_pair;
  } u;
};

typedef yeah* (*foreign_function)(yeah* arg);

yeah* store_global(char *s, yeah* v);
yeah* closure(yeah* lambda, yeah* env);
yeah* lambda(yeah* arg, yeah* body);
yeah* thunk(yeah* exp, yeah* env);
yeah* symbol(char* s);
yeah* csymbol(char* s);
yeah* string(char* s);
yeah* opaque(void* q);
yeah* pair(yeah* car, yeah* cdr);
yeah* integer(int i);
yeah* app(yeah* f, yeah* arg);
yeah* nil(void);
int getint(yeah* e);
void evl_top(char* src, yeah* e);
char* stringval(yeah* e);
void* opaque_val(yeah* y);
yeah* hcar(yeah* y);
yeah* hcdr(yeah* y);
yeah* hcadr(yeah* y);
void dump(yeah* y);
void dumpn(yeah* y);
void tdumpn(char* s, yeah* y);
bool isnil(yeah* e);
yeah* car(yeah* e);
yeah* cdr(yeah* e);

extern yeah *Nil, *CNil;

void register_command(char *name, foreign_function f);

