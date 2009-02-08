/* $Id: mem.h,v 1.1 2004/08/06 14:14:29 greg Exp $ */

#ifndef _mem_h_
#define _mem_h_

#define malik(size) (malik_actual(__FILE__,__LINE__,(size)))
#define realik(mem,size) (realik_actual(__FILE__,__LINE__,(mem),(size)))
#define fri(mem) (fri_actual(__FILE__,__LINE__,(mem)))

#define NEW(str) ((str *)cmalik( sizeof(str) ))
#define NEWC(str,n) ((str *)cmalik( sizeof(str)*(n) ))
#define NCNEW(str) ((str *)malik( sizeof(str) ))
#define NCNEWC(str,n) ((str *)malik( sizeof(str)*(n) ))

void *cmalik( int size );

void *malik_actual( char *file, int line, int size );
void *realik_actual( char *file, int line, void *mem, int size );
void fri_actual( char *file, int line, void *mem );
void memkapi( void *dest, void *src, int len );
void strkapi( char *dest, char *src );
void *memfill( void *dest, int c, int n );

char *strdoop( char *s );
char *strkat( char *s0, char *s1 );

#define MEM_NOT_ADDRESS ((void*)-1)
extern void *mem_min_addr;
extern void *mem_max_addr;
#define MEM_ADD_RANGE_IS_SET() \
  (mem_min_addr!=MEM_NOT_ADDRESS)
#define MEM_ADDR_IN_RANGE(m)      \
  ( (MEM_ADD_RANGE_IS_SET()) &&   \
    ((void*)(m))>=mem_min_addr && \
    ((void*)(m))<=mem_max_addr )
#define AMEM_ADDR_IN_RANGE(m)      \
  RA( MEM_ADDR_IN_RANGE((m)),   \
      ("Address %x out of range %x..%x\n", (m), mem_min_addr, mem_max_addr) )

#endif /* _mem_h_ */
