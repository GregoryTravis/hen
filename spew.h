/* $Id: spew.h,v 1.3 2005/02/18 20:20:34 greg Exp $ */

#ifndef _spew_h_
#define _spew_h_

#define SPEW_STREAM stdout

#define SPEW_ON

#ifdef SPEW_ON

#define spew(a) spew_do a
#define spew_tab(s,t) spew_tab_do(s,t)

void spew_do( char *f, ... );
void spew_tab_do( int tab );

#define SPEWB(code) do code while (0)

#else

#define spew(a)
#define spew_tab(s,t)

#define SPEWB(code) do {} while (0)

#endif

#ifdef NOLINE
# define SPEWLINE (0)
#else
# define SPEWLINE __LINE__
#endif

#define SPEW_LOCATION() \
  spew(( "At %s:%d:\n", __FILE__, SPEWLINE ))

#define err(a) (SPEW_LOCATION(),err_do a)
#define warn(a) (SPEW_LOCATION(),warn_do a)

void warn_do( char *f, ... );
void err_do( char *f, ... );

void bkpt( void );

#define here() spew(( "at %s:%d\n", __FILE__, __LINE__ ))

#endif /* _spew_h_ */
