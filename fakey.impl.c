#include <glut.h>

#include "a.h"
#include "mem.h"

#include "vor.h"

#include "fakey.impl.h"

yeah* fakey_impl_glutInitDisplayMode(yeah* arg) {
  A(isnil(hcdr(arg)));
  int mode = getint(hcar(arg));
  glutInitDisplayMode(mode);
  return Nil;
}

void fakey_impl_register() {
  register_command("fakey_impl_glutInitDisplayMode", &fakey_impl_glutInitDisplayMode);
}
