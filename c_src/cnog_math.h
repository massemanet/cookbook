/*  this file is generated */
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include "cnog.h"

/*******************************/
bool cnog_math_cos(int ARI, ei_x_buff *XBUF, char *B, int *I){


  double angle;
  double R; /* return value */

  if ( ! gn_check_arity(XBUF, 1, ARI) ) return FALSE;
  if ( ! gn_get_arg_double(XBUF, B, I, &angle) ) return FALSE;
  R = cos(angle);
  gn_put_double(XBUF,(double)R);
  return TRUE;
}
