#ifndef __blip_h_
#define __blip_h_

#include "a.h"
#include "yeah.h"

void dump(yeah* y);
bool samesymbol(yeah* a, yeah* b);

#define binop_decl(_name, _op) yeah* __##_name(yeah* args)
#define binop_def(_name, _op) binop_decl(_name, _op) { yeah* a; yeah* b; listmatch2(args, &a, &b); A(isnumber(a)); A(isnumber(b)); return mknumber(a->u.number.d _op b->u.number.d); }

binop_decl(plus, +);
binop_decl(minus, -);

#endif // __blip_h_
