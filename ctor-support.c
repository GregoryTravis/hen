#include <stdio.h>
#include <string.h>

// TODO: REALLY bad way to be reading lines.
#define CTOR_READBUF_SIZE 2048
static char ctor_readbuf[CTOR_READBUF_SIZE];

// TODO: REALLY bad way to be reading strings.
static char badbad[2048];

static char* readline(FILE *fp) {
  char *foop = fgets(ctor_readbuf, CTOR_READBUF_SIZE, fp);
  foop[strlen(foop) - 1] = 0;

  if (foop == NULL) {
    printf("DONE\n");
    err(("Bad input line."));
  } else {
    //printf("LINE [%s]\n", ctor_readbuf);
    return ctor_readbuf;
  }
 }

static int readint(FILE *fp) {
  int n;
  A(sscanf(readline(fp), "%d", &n) == 1);
  return n;
}

static char* readstring(FILE *fp) {
  char* line = readline(fp);
  A(line);
  return strdup(line);
}
