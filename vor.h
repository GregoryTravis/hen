#include "a.h"

typedef enum {
  LAMBDA,
  CLOSURE,
  THUNK,
  INTEGER,
  FLOAT,
  UCHAR,
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
      float f;
    } flote;

    struct {
      uchar uc;
    } uchair;

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
yeah* flote(float f);
yeah* uchair(uchar uc);
yeah* app(yeah* f, yeah* arg);
yeah* nil(void);
int getint(yeah* e);
float getfloat(yeah* e);
// TODO use uchar
uchar getuchar(yeah* e);
void evl_top(char* src, yeah* e);
char* stringval(yeah* e);
void* opaqueval(yeah* y);
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

void evl_from_callback(yeah* e);

typedef void (*vvfunp)(void);
typedef void (*viifunp)(int, int);
typedef void (*vuciifunp)(unsigned char, int, int);

// TODO generate these
vvfunp wrap_hen_fun_vvfunp(yeah* f);
viifunp wrap_hen_fun_viifunp(yeah* f);
vuciifunp wrap_hen_fun_vuciifunp(yeah* f);
