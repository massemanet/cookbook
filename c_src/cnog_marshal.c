#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include "ei.h"
#include "cnog.h"

/***************************************************************************/
/* wrap all answers in this */
/* {{self(),reply|signal|error|handshake},...} */
/***************************************************************************/

void cnog_wrap_ans(char* tag, ei_x_buff* xbuf) {
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_pid(xbuf, cnog_self());
  ei_x_encode_atom(xbuf, tag);
}

void cnog_wrap_reply(char* tag, ei_x_buff* xbuf) {
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_atom(xbuf, tag);
}
/***************************************************************************/
/* error tuple builders */
/***************************************************************************/
/* {{Node,error}, {Err,...}} */
void cnog_enc_2_error(ei_x_buff *xbuf, char *err) {
  cnog_wrap_reply("error", xbuf);
  ei_x_encode_tuple_header(xbuf,2);
  ei_x_encode_atom(xbuf, err);
}

/* {{Node,error}, Err} */
void cnog_enc_1_error(ei_x_buff *xbuf, char *err) {
  cnog_wrap_reply("error", xbuf);
  ei_x_encode_atom(xbuf, err);
}

/***************************************************************************/
/* marshal return values from C calls */
/* {{Node,reply}, ...} */
/***************************************************************************/
void cnog_put_tuple(ei_x_buff *xbuf, int N) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_tuple_header(xbuf,N));
}

void cnog_put_void(ei_x_buff *xbuf) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_atom(xbuf, "void") );
}

void cnog_put_pid(ei_x_buff *xbuf, erlang_pid *p) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_pid(xbuf, p) );
}

void cnog_put_boolean(ei_x_buff *xbuf, int b) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_boolean(xbuf, b) );
}
void cnog_put_bool(ei_x_buff *xbuf, int b) {
  cnog_put_boolean(xbuf, b);
}

void cnog_put_double(ei_x_buff *xbuf, double d) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_double(xbuf, d) );
}
void cnog_put_float(ei_x_buff *xbuf, float f) {
  cnog_put_double(xbuf, (double)f);
}

void cnog_put_longlong(ei_x_buff *xbuf, long long l) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_longlong(xbuf, l) );
}
void cnog_put_long(ei_x_buff *xbuf, long i) {
  cnog_put_longlong(xbuf, (long long)i);
}
void cnog_put_int(ei_x_buff *xbuf, int i) {
  cnog_put_longlong(xbuf, (long long)i);
}
void cnog_put_short(ei_x_buff *xbuf, short i) {
  cnog_put_longlong(xbuf, (long long)i);
}

void cnog_put_ulonglong(ei_x_buff *xbuf, unsigned long long l) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_ulonglong(xbuf, l) );
}
void cnog_put_ulong(ei_x_buff *xbuf, unsigned long i) {
  cnog_put_ulonglong(xbuf, (unsigned long long)i);
}
void cnog_put_uint(ei_x_buff *xbuf, unsigned int i) {
  cnog_put_ulonglong(xbuf, (unsigned long long)i);
}
void cnog_put_ushort(ei_x_buff *xbuf, unsigned short i) {
  cnog_put_ulonglong(xbuf, (unsigned long long)i);
}

void cnog_put_atom(ei_x_buff *xbuf, char *p) {
  cnog_wrap_reply("ok", xbuf);
  assert( ! ei_x_encode_atom(xbuf, p) );
}

void cnog_put_string(ei_x_buff *xbuf, char *p) {
  cnog_wrap_reply("ok", xbuf);
  if ( p == NULL ) {
    ei_x_encode_string(xbuf, "");
  }else{
    ei_x_encode_string(xbuf, p);
  }
}

/***************************************************************************/
/* marshalling erlang -> C */
/***************************************************************************/
bool cnog_get_arg_pid(ei_x_buff *xbuf, char *B, int *I, erlang_pid *pid){
  if ( ! ei_decode_pid(B ,I, pid) ) return true;
  cnog_enc_1_error(xbuf, "bad_pid");
  return false;
}

bool cnog_get_arg_boolean(ei_x_buff *xbuf, char *B, int *I, int *b) {
  if ( ! ei_decode_boolean(B, I, b) ) return true;
  cnog_enc_1_error(xbuf, "bad_boolean");
  return false;
}

bool cnog_get_arg_atom(ei_x_buff *xbuf, char *B, int *I, char *a){
  if ( ! ei_decode_atom(B, I, a) ) return true;
  cnog_enc_1_error(xbuf, "bad_atom");
  return false;
}

/* if cnog_get_arg_string returns true, *a must be freed */
bool cnog_get_arg_string(ei_x_buff *xbuf, char *B, int *I, char **a){
  int type, size;

  ei_get_type(B, I, &type, &size); /* can't fail */
  *a = (char *)malloc(size+1);
  if ( ! ei_decode_string(B, I, *a) ) return true;
  free(*a);
  cnog_enc_1_error(xbuf, "bad_string");
  return false;
}

bool cnog_get_arg_longlong(ei_x_buff *xbuf, char *B, int *I, long long *a) {
  int type, size;

  ei_get_type(B, I, &type, &size); /* can't fail */
  switch (type) {
  case '#':
    *a = *(B + *I + *(B + *I + 1) + 3);
    (*(B + *I + 1))++;
    if (*(B + *I + 1) == *(B + *I + 2)) {
      *I += *(B + *I + 2) + 3;
    }
    return true;
  default:
    if ( ! ei_decode_longlong(B ,I, a) ) return true;
    cnog_enc_1_error(xbuf, "bad_integer");
    return false;
  }
}
bool cnog_get_arg_long(ei_x_buff *xbuf, char *B, int *I, long *a) {
  long long ll;

  if ( ! cnog_get_arg_longlong(xbuf, B, I, &ll) ) return false;
  *a = ll;
  return true;
}
bool cnog_get_arg_int(ei_x_buff *xbuf, char *B, int *I, int *a) {
  long long ll;

  if ( ! cnog_get_arg_longlong(xbuf, B, I, &ll) ) return false;
  *a = ll;
  return true;
}
bool cnog_get_arg_short(ei_x_buff *xbuf, char *B, int *I, short *a) {
  long long ll;

  if ( ! cnog_get_arg_longlong(xbuf, B, I, &ll) ) return false;
  *a = ll;
  return true;
}

bool cnog_get_arg_ulonglong(ei_x_buff *xbuf, char *B, int *I,
                            unsigned long long *ull) {
  if ( ! ei_decode_ulonglong(B, I, ull) ) return true;
  cnog_enc_1_error(xbuf, "bad_unsigned_integer");
  return false;
}
bool cnog_get_arg_ushort(ei_x_buff *xbuf, char *B, int *I, unsigned short *a) {
  unsigned long long ull;

  if ( ! cnog_get_arg_ulonglong(xbuf,B,I,&ull) ) return false;
  *a = ull;
  return true;
}
bool cnog_get_arg_uint(ei_x_buff *xbuf, char *B, int *I, unsigned int *a) {
  unsigned long long ull;

  if ( ! cnog_get_arg_ulonglong(xbuf,B,I,&ull) ) return false;
  *a = ull;
  return true;
}
bool cnog_get_arg_ulong(ei_x_buff *xbuf, char *B, int *I, unsigned long *a) {
  unsigned long long ull;

  if ( ! cnog_get_arg_ulonglong(xbuf,B,I,&ull) ) return false;
  *a = ull;
  return true;
}

bool cnog_get_arg_double(ei_x_buff *xbuf, char *B, int *I, double *a) {
  if ( ! ei_decode_double(B, I, a) ) return true;
  cnog_enc_1_error(xbuf, "bad_double");
  return false;
}
bool cnog_get_arg_float(ei_x_buff *xbuf, char *B, int *I, float *a) {
  double dbl;

  if ( ! cnog_get_arg_double(xbuf, B, I, &dbl) ) return false;
  *a = dbl;
  return true;
}

bool cnog_check_arity(ei_x_buff *xbuf, int a1, int a2) {
  if ( a1 == a2 ) return true;
  cnog_enc_2_error(xbuf, "bad_arity");
  ei_x_encode_long(xbuf, (long) a2);
  return false;
}

/***************************************************************************/
int cnog_get_list(ei_x_buff *xbuf, char *B, int *I) {
  int ari;
  int type, size;

  ei_get_type(B, I, &type, &size); /* can't fail */
  switch (type) {
  case ERL_LIST_EXT:
  case ERL_NIL_EXT:
    if ( ! ei_decode_list_header(B, I, &ari) ) return ari;
    cnog_enc_1_error(xbuf, "bad_list");
    break;
  case ERL_STRING_EXT:
    *(B + *I) = '#';
    assert(*(B + *I + 1) == 0);
    return size;
    break;
  }
  return -1;
}

int cnog_get_tuple(ei_x_buff *xbuf, char *B, int *I) {
  int ari;

  if ( ! ei_decode_tuple_header(B, I, &ari) ) return ari;
  cnog_enc_1_error(xbuf, "bad_tuple");
  return -1;
}
/* 'a' ERL_SMALL_INTEGER_EXT */
/* 'b' ERL_INTEGER_EXT       */
/* 'c' ERL_FLOAT_EXT         */
/* 'd' ERL_ATOM_EXT          */
/* 'e' ERL_REFERENCE_EXT     */
/* 'f' ERL_PORT_EXT          */
/* 'g' ERL_PID_EXT           */
/* 'h' ERL_SMALL_TUPLE_EXT   */
/* 'i' ERL_LARGE_TUPLE_EXT   */
/* 'j' ERL_NIL_EXT           */
/* 'k' ERL_STRING_EXT        */
/* 'l' ERL_LIST_EXT          */
/* 'm' ERL_BINARY_EXT        */
/* 'n' ERL_SMALL_BIG_EXT     */
/* 'o' ERL_LARGE_BIG_EXT     */
/* 'p' ERL_NEW_FUN_EXT       */
/* 'r' ERL_NEW_REFERENCE_EXT */
/* 's' ERL_SMALL_ATOM_EXT    */
/* 'u' ERL_FUN_EXT           */
/* 'v' ERL_ATOM_UTF8_EXT     */
/* 'w' ERL_SMALL_ATOM_UTF8_EXT */
/* 'C' ERL_CACHED_ATOM       */
/* 'N' ERL_NEW_CACHE         */
