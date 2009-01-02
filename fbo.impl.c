#include "a.h"
#include "mem.h"

#include "vor.h"

#include "fbo.h"
#include "fbo.impl.h"

yeah* fbo_impl_fbo_main0(yeah* arg) {
  A(isnil(arg));
  fbo_main0();
  return Nil;
}

yeah* fbo_impl_fbo_main1(yeah* arg) {
  A(isnil(arg));
  fbo_main1();
  return Nil;
}

void fbo_impl_register() {
  register_command("fbo_impl_fbo_main0", &fbo_impl_fbo_main0);
  register_command("fbo_impl_fbo_main1", &fbo_impl_fbo_main1);
}
