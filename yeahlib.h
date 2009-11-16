/* $Id$ */

#ifndef _yeahlib_h_
#define _yeahlib_h_

#include "yeah.h"

yeah* car(yeah* y);
yeah* cdr(yeah* y);
yeah* cons(yeah* a, yeah* d);
yeah* cadr(yeah* y);
yeah* cddr(yeah* y);

int length(yeah* y);

yeah* nth(yeah* y, int n);

yeah* map(yeah* (*f)(yeah*), yeah* list);

bool isthissymbol(yeah* o, char *name);

#endif /* _yeahlib_h_ */
