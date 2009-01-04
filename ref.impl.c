#include <stdio.h>

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

yeah* create_null_ref(yeah* arg) {
  A(isnil(arg));
  return opaque(NULL);
}

void ref_impl_register() {
  register_command("create-int-ref-impl", &create_int_ref);
  register_command("read-int-ref-impl", &read_int_ref);
  register_command("write-int-ref-impl", &write_int_ref);
  register_command("destroy-int-ref-impl", &destroy_int_ref);
  register_command("create-null-ref-impl", &create_null_ref);
}
