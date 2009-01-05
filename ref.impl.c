#include <stdio.h>
#include <string.h>

#include "a.h"
#include "mem.h"

#include "vor.h"

#include "ref.impl.h"

// HEY naming convention
yeah* create_int_ref(yeah* arg) {
  int* ip = NEW(int);
  *ip = getint(hcar(arg));
  return opaque(ip);
}

yeah* read_int_ref(yeah* arg) {
  int* ip = (int*)opaqueval(hcar(arg));
  A(ip);
  return integer(*ip);
}

yeah* write_int_ref(yeah* arg) {
  int* ip = (int*)opaqueval(hcar(arg));
  A(ip);
  int i = getint(hcadr(arg));
  *ip = i;
  return Nil;
}

yeah* destroy_int_ref(yeah* arg) {
  fri(opaqueval(hcar(arg)));
  return Nil;
}

// HEY naming convention
yeah* create_float_ref(yeah* arg) {
  float* ip = NEW(float);
  *ip = getfloat(hcar(arg));
  return opaque(ip);
}

yeah* read_float_ref(yeah* arg) {
  float* ip = (float*)opaqueval(hcar(arg));
  A(ip);
  return flote(*ip);
}

yeah* write_float_ref(yeah* arg) {
  float* ip = (float*)opaqueval(hcar(arg));
  A(ip);
  float i = getfloat(hcadr(arg));
  *ip = i;
  return Nil;
}

yeah* destroy_float_ref(yeah* arg) {
  fri(opaqueval(hcar(arg)));
  return Nil;
}

// HEY naming convention
yeah* create_charp_ref(yeah* arg) {
  char** ip = NEW(char*);
  *ip = strdup(stringval(hcar(arg)));
  return opaque(ip);
}

yeah* read_charp_ref(yeah* arg) {
  char** ip = (char**)opaqueval(hcar(arg));
  A(ip);
  return string(*ip);
}

yeah* write_charp_ref(yeah* arg) {
  char** ip = (char**)opaqueval(hcar(arg));
  A(ip);
  // TODO s, not i; and for float, above.
  char* i = strdup(stringval(hcadr(arg)));
  *ip = i;
  return Nil;
}

yeah* destroy_charp_ref(yeah* arg) {
  fri(opaqueval(hcar(arg)));
  return Nil;
}

yeah* create_null_ref(yeah* arg) {
  A(isnil(arg));
  return opaque(NULL);
}

void ref_impl_register() {
  register_command("create-int-ref-impl", &create_int_ref);
  register_command("read-int-ref-impl", &read_int_ref);
  register_command("write-int-ref-impl", &write_int_ref);
  register_command("destroy-int-ref-impl", &destroy_int_ref);
  register_command("create-float-ref-impl", &create_float_ref);
  register_command("read-float-ref-impl", &read_float_ref);
  register_command("write-float-ref-impl", &write_float_ref);
  register_command("destroy-float-ref-impl", &destroy_float_ref);
  register_command("create-charp-ref-impl", &create_charp_ref);
  register_command("read-charp-ref-impl", &read_charp_ref);
  register_command("write-charp-ref-impl", &write_charp_ref);
  register_command("destroy-charp-ref-impl", &destroy_charp_ref);
  register_command("create-null-ref-impl", &create_null_ref);
}
