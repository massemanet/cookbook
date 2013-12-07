/*  this file is generated */
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include "cnog.h"

/*******************************/
bool cnog_math_cos(int ARI, ei_x_buff *XBUF, char *B, int *I){


  double angle;
  double R; /* return value */

  if ( ! cnog_check_arity(XBUF, 1, ARI) ) return false;
  if ( ! cnog_get_arg_double(XBUF, B, I, &angle) ) return false;
  R = cos(angle);
  cnog_put_double(XBUF,(double)R);
  return true;
}
