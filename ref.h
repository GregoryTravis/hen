#include <math.h>
#define DEKKIE(_type) _type* create_##_type##_ref(_type i); _type read_##_type##_ref(_type* ip); void write_##_type##_ref(_type* ip, _type i); void destroy_##_type##_ref(_type* ip); 
DEKKIE(int)
DEKKIE(float)
