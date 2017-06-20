/* 
   C implementation for float.ml module.

   Copyright (C) 2011 Antoine Miné
*/

#include <math.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <fenv.h>

/* init */

value ml_float_init(value dummy)
{
  CAMLparam0();
  fesetround(FE_UPWARD);
  CAMLreturn(Val_unit);
}


/* single precision operations */

double ml_add_flt_f(double a, double b)
{
  return (float)(a + b);
}

double ml_sub_flt_f(double a, double b)
{
  return (float)(a - b);
}

double ml_mul_flt_f(double a, double b)
{
  return (float)(a * b);
}

double ml_div_flt_f(double a, double b)
{
  return (float)(a / b);
}

value ml_add_flt(value a, value b)
{
  CAMLparam2(a,b);
  CAMLreturn(copy_double((float)(Double_val(a) + Double_val(b))));
}

value ml_sub_flt(value a, value b)
{
  CAMLparam2(a,b);
  CAMLreturn(copy_double((float)(Double_val(a) - Double_val(b))));
}

value ml_mul_flt(value a, value b)
{
  CAMLparam2(a,b);
  CAMLreturn(copy_double((float)(Double_val(a) * Double_val(b))));
}

value ml_div_flt(value a, value b)
{
  CAMLparam2(a,b);
  CAMLreturn(copy_double((float)(Double_val(a) / Double_val(b))));
}

double ml_round_flt_f(double a)
{
  return (float)a;
}

value ml_round_flt(value a)
{
  CAMLparam1(a);
  CAMLreturn(copy_double((float)(Double_val(a))));
}

double ml_of_int_flt_f(value a)
{
  return (float)Long_val(a);
}
value ml_of_int_flt(value a)
{
  CAMLparam1(a);
  CAMLreturn(copy_double((float)(Long_val(a))));
}
