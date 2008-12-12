/* $Id: mem.c,v 1.1 2004/08/06 14:14:29 greg Exp $ */

#include <stdlib.h>
#include <string.h>
#include "a.h"
#include "mem.h"
#include "spew.h"

void *mem_min_addr=MEM_NOT_ADDRESS;
void *mem_max_addr=MEM_NOT_ADDRESS;

static int verbose = 0;

void *malik_last( int size )
{
  void *d = malloc( size );

  return d;
}

void *realik_last( void *mem, int size )
{
  return realloc( mem, size );
}

void fri_last( void *mem )
{
  free( mem );
}

void *malik_actual( char *file, int line, int size )
{
  void *mem;

  mem = malik_last( size );

  if (verbose) {
    spew(( "malik %x size %d file %s line %d\n", mem, size, file, line ));
  }

  if (mem==0) {
    err(( "malik: Out of memory allocating %d\n", size ));
  }

  // Track min/max
  if (mem < mem_min_addr) {
    mem_min_addr = mem;
  }
  if (mem+size-1 > mem_max_addr) {
    mem_max_addr = mem+size-1;
  }

  return mem;
}

/* This might not work right if we turn freeing off! Because it
   might call free. */
void *realik_actual( char *file, int line, void *mem, int size )
{
  void *rmem;

  rmem = realik_last( mem, size );

  if (verbose) {
    spew(( "realik %x size %d was %x file %s line %d\n",
           rmem, size, mem, file, line ));
  }

  if (rmem==0) {
    err(( "realik: Out of memory allocating %d\n", size ));
  }

  return rmem;
}

void fri_actual( char *file, int line, void *mem )
{
  if (verbose) {
    spew(( "fri %x file %s line %d\n", mem, file, line ));
  }

  fri_last( mem );
}

void memkapi( void *dest, void *src, int len )
{
  memcpy( dest, src, len );
}

void strkapi( char *dest, char *src )
{
  strcpy( dest, src );
}

void *memfill( void *dest, int c, int n )
{
  return memset( dest, c, n );
}

char *strdoop( char *s )
{
  char *ss = (char*)malik( strlen( s )+1 );
  strcpy( ss, s );

  if (verbose) {
    spew(( "strdup 0x%x <- 0x%x\n", ss, s ));
  }

  return ss;
}

void *cmalik( int size )
{
  void *mem = malik( size );
  memfill( mem, 0, size );
  return mem;
}

char *strkat( char *s0, char *s1 )
{
  int len0 = strlen( s0 );
  int len1 = strlen( s1 );

  char *c = malik( len0+len1+1 );
  strkapi( c, s0 );
  strkapi( c+len0, s1 );

  return c;
}
