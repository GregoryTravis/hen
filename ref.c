#include "mem.h"
#include "ref.h"

//int* create_int_ref(int i) {int* ip = NEW(int); *ip = i; return ip;} int read_int_ref(int* ip) {return *ip;} void write_int_ref(int* ip, int i) {*ip = i;} void destroy_int_ref(int* ip ) {fri(ip);} 

#define REFFIE(_type) _type* create_##_type##_ref(_type i) {_type* ip = NEW(_type); *ip = i; return ip;} _type read_##_type##_ref(_type* ip) {return *ip;} void write_##_type##_ref(_type* ip, _type i) {*ip = i;} void destroy_##_type##_ref(_type* ip ) {fri(ip);} 

REFFIE(int)
REFFIE(float)
//REFFIE(charp)

void* create_null_ref(void) {
  return 0;
}
