#include "mem.h"

int* create_int_ref(int i) {
  int* ip = NEW(int);
  *ip = i;
  return ip;
}

int read_int_ref(int* ip) {
  return *ip;
}

void write_int_ref(int* ip, int i) {
  *ip = i;
}

void destroy_int_ref(int* ip ) {
  fri(ip);
}
