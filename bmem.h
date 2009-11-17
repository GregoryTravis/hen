// $Id$

#ifndef _bmem_h_
#define _bmem_h_

void* bmalik(int n);
#define BNEW(str) ((str*)bmalik(sizeof(str)))

#endif /* _bmem_h_ */
