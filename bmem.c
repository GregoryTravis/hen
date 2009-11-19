// $Id$

#include <stdlib.h>
#include "a.h"
#include "bmem.h"

#define BLOCKSIZE (1024 * 1024)

static void* current_block = NULL;
static int bytes_left = 0;

void* bmalik(int n) {
  if (n > bytes_left) {
    A(n <= BLOCKSIZE);

    current_block = malloc(BLOCKSIZE);
    A(current_block);
    bytes_left = BLOCKSIZE;
  }

  void* ret = current_block;
  bytes_left -= n;
  current_block += n;

  return ret;
}
