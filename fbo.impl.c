#include <glut.h>

#include "a.h"
#include "mem.h"

#include "vor.h"

#include "fbo.h"
#include "fbo.impl.h"

yeah* fbo_main0(yeah* arg) {
  A(isnil(arg));
  fbo_main0_();
  return Nil;
}

yeah* glutInitDisplayMode0(yeah* arg) {
  int mode = getint(arg);
  glutInitDisplayMode(mode);
  return Nil;
}

yeah* fbo_main1(yeah* arg) {
  A(isnil(arg));
  fbo_main1_();
  return Nil;
}

void fbo_impl_register() {
  register_command("fbo_main0", &fbo_main0);
  register_command("glutInitDisplayMode0", &glutInitDisplayMode0);
  register_command("fbo_main1", &fbo_main1);
}
