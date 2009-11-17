// $Id$

#include <stdlib.h>
#include "a.h"
#include "bmem.h"

#define BLOCKSIZE (1024 * 1024)

static void* current_block = NULL;
static int block_cursor = 0;

void* bmalik(int n) {
  if (current_block == NULL || block_cursor + n > BLOCKSIZE) {
    A(n <= BLOCKSIZE);

    current_block = malloc(BLOCKSIZE);
    A(current_block);
    block_cursor = 0;
  }

  void* ret = current_block + block_cursor;
  block_cursor += n;

  return ret;
}
