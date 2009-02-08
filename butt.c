#include "butt.h"

#include <stdio.h>
#include <string.h>

#include "a.h"
#include "mem.h"

int* create_butt(int v) {
  int* vp = NEW(int);
  *vp = v;
  return vp;
}

int read_butt(int* vp) {
  A(vp);
  return *vp;
}

void write_butt(int* vp, int v) {
  A(vp);
  *vp = v;
}

void destroy_butt(int* vp) {
  fri(vp);
}

#if 0
double* create_dubble(double v) {
  double* vp = NEW(double);
  *vp = v;
  return vp;
}

double read_dubble(double* vp) {
  A(vp);
  return *vp;
}

void write_dubble(double* vp, double v) {
  A(vp);
  *vp = v;
}

void destroy_dubble(double* vp) {
  fri(vp);
}
#endif

double joe_add(double a, double b) {
  return a + b;
}
