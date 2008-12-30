#include <stdio.h>

#include "vor.h"

#include "shew.impl.h"

yeah* shew(yeah* arg) {
  dump(arg);
  printf("\n");
  return CNil;
}

void shew_impl_register() {
  register_command("shew", &shew);
}
