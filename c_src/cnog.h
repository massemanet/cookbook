#include "ei.h"

typedef struct {
  erlang_pid pid;
  int fd;
} cnog_dest;

erlang_pid *cnog_self(void);

void cnog_wrap_ans(char* tag, ei_x_buff* xbuf);
void cnog_wrap_reply(char* tag, ei_x_buff* xbuf);
void cnog_enc_2_error(ei_x_buff *xbuf, char *err);
void cnog_enc_1_error(ei_x_buff *xbuf, char *err);
void cnog_put_tuple(ei_x_buff *xbuf, int N);
void cnog_put_void(ei_x_buff *xbuf);
void cnog_put_pid(ei_x_buff *xbuf, erlang_pid *p);
void cnog_put_boolean(ei_x_buff *xbuf, int b);
void cnog_put_double(ei_x_buff *xbuf, double d);
void cnog_put_longlong(ei_x_buff *xbuf, long long l);
void cnog_put_ulonglong(ei_x_buff *xbuf, unsigned long long l);
void cnog_put_atom(ei_x_buff *xbuf, char *p);
void cnog_put_string(ei_x_buff *xbuf, char *p);
bool cnog_get_arg_pid(ei_x_buff *xbuf, char *B, int *I, erlang_pid *pid);
bool cnog_get_arg_boolean(ei_x_buff *xbuf, char *B, int *I, int *b);
bool cnog_get_arg_atom(ei_x_buff *xbuf, char *B, int *I, char *a);
bool cnog_get_arg_string(ei_x_buff *xbuf, char *B, int *I, char **a);
bool cnog_get_arg_short(ei_x_buff *xbuf, char *B, int *I, short *a);
bool cnog_get_arg_int(ei_x_buff *xbuf, char *B, int *I, int *a);
bool cnog_get_arg_long(ei_x_buff *xbuf, char *B, int *I, long *a);
bool cnog_get_arg_longlong(ei_x_buff *xbuf, char *B, int *I, long long *a);
bool cnog_get_arg_ushort(ei_x_buff *xbuf, char *B, int *I, unsigned short *a);
bool cnog_get_arg_uint(ei_x_buff *xbuf, char *B, int *I, unsigned int *a);
bool cnog_get_arg_ulong(ei_x_buff *xbuf, char *B, int *I, unsigned long *a);
bool cnog_get_arg_ulonglong(ei_x_buff *xbuf, char *B, int *I,
                            unsigned long long *ull);
bool cnog_get_arg_float(ei_x_buff *xbuf, char *B, int *I, float *a);
bool cnog_get_arg_double(ei_x_buff *xbuf, char *B, int *I, double *a);
bool cnog_check_arity(ei_x_buff *xbuf, int a1, int a2);
int cnog_get_list(ei_x_buff *xbuf, char *B, int *I);
int cnog_get_tuple(ei_x_buff *xbuf, char *B, int *I);
