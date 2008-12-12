/* $Id: a.h,v 1.1 2004/08/06 14:11:19 greg Exp $ */

#ifndef _a_h_
#define _a_h_

#ifdef NOLINE
# define ALINE (0)
#else
# define ALINE __LINE__
#endif

#include "spew.h"

typedef unsigned char bool;
typedef unsigned char uchar;

#define ASSERT

#define _A(expr) ((expr) ? 0 : \
  (err(( "Assert %s %d: %s\n", __FILE__, ALINE, #expr )),0) )
#define _RA(expr,rept)                                       \
  (                                                          \
    (expr) ?  0 :                                            \
      (spew(( "Assertion failure:\n") ),                     \
       err( rept ),                                          \
       0)                                                    \
  )

#ifdef ASSERT

#define A(expr) _A(expr)
#define RA(expr,rept) _RA(expr,rept)

#else

#define A(expr) 0
#define RA(expr,rept) 0

#endif

#define AA(expr) _A(expr)
#define RAA(expr,rept) _RA(expr,rept)

#endif /* _a_h_ */
