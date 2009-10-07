/* spew.c */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "a.h"
#include "mem.h"
#include "spew.h"

void vspew( char *f, va_list va );

void bkpt( void )
{
}

void spew_do( char *f, ... )
{
    va_list va;

    va_start(va,f);
    vspew( f, va );
    va_end(va);
}

void vspew( char *f, va_list va )
{
  vfprintf( SPEW_STREAM, f, va );
  fflush( SPEW_STREAM );
}

void warn_do( char *f, ... )
{
    va_list va;
    va_start(va,f);
    vspew( f, va );
    va_end( va );
}

void err_do( char *f, ... )
{
    va_list va;
    spew(( "Error: " ));
    va_start(va,f);
    vspew( f, va );
    va_end( va );
    spew(( "\n" ));
    fflush( stdout );
    fflush( stderr );
    bkpt();
    exit( 1 );
}

void spew_tab_do( int tab )
{
  while (tab--)
    spew(( " " ));
}
