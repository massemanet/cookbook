#include <assert.h>
#include <dlfcn.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "cnog.h"
#include "example.h"

// variable - Foo::double, setter
bool wrap__set_Foo__(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  if ( ! cnog_check_arity(XBUF, 1, ARI) ) return false;
  if ( ! cnog_get_arg_double(XBUF, B, I, &Foo) ) return false;
  cnog_put_void(XBUF);
  return true;
}
// variable - Foo::double, getter
bool wrap__get_Foo__(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  if ( ! cnog_check_arity(XBUF, 0, ARI) ) return false;
  cnog_put_double(XBUF,(double)Foo);
  return true;
}
// function - name : pie, action: result = (float)pie();
bool wrap_pie(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  float RESULT;
  if ( ! cnog_check_arity(XBUF, 0, ARI) ) return false;
  RESULT = pie();
  cnog_put_double(XBUF,(double)RESULT);
  return true;
}
// function - name : prime, action: result = (bool)prime(arg1);
bool wrap_prime(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  bool RESULT;
  if ( ! cnog_check_arity(XBUF, 1, ARI) ) return false;
  int n;
  if ( ! cnog_get_arg_int(XBUF, B, I, &n) ) return false;
  RESULT = prime(n);
  cnog_put_boolean(XBUF,(bool)RESULT);
  return true;
}
// function - name : gcd, action: result = (int)gcd(arg1,arg2);
bool wrap_gcd(int ARI, ei_x_buff *XBUF, char *B, int *I) {
  int RESULT;
  if ( ! cnog_check_arity(XBUF, 2, ARI) ) return false;
  int x;
  if ( ! cnog_get_arg_int(XBUF, B, I, &x) ) return false;
  int y;
  if ( ! cnog_get_arg_int(XBUF, B, I, &y) ) return false;
  RESULT = gcd(x, y);
  cnog_put_longlong(XBUF,(long long)RESULT);
  return true;
}

