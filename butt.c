#include "butt.h"

#include <stdio.h>
#include <string.h>

#include "a.h"
#include "mem.h"

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif
