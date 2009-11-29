#include <math.h>

#ifndef _ref_h_
#define _ref_h_

#define poop 111

typedef char* charp;

#define DEKKIE(_type) _type* create_##_type##_ref(_type i); _type read_##_type##_ref(_type* ip); void write_##_type##_ref(_type* ip, _type i); void destroy_##_type##_ref(_type* ip); 
DEKKIE(int)
DEKKIE(float)
//DEKKIE(charp)

void* create_null_ref(void);

#endif  // _ref_h_
