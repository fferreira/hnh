#ifndef _INTRINSINC_H_
#define _INTRINSINC_H_

#include "runtime.h"

#include <assert.h>
#include <string.h>

// Integer Functions

// Arithmetic Functions

static __inline__ value * int_add(value * a, value * b)
{
  return alloc_int(a->int_value + b->int_value, &front_seg); 
}

static __inline__ value * int_sub(value * a, value * b)
{
  return alloc_int(a->int_value - b->int_value, &front_seg); 
}

static __inline__ value * int_mul(value * a, value * b)
{
  return alloc_int(a->int_value * b->int_value, &front_seg); 
}

static __inline__ value * int_div(value * a, value * b)
{
  return alloc_int(a->int_value / b->int_value, &front_seg); 
}

static __inline__ value * int_neg(value * a)
{
  return alloc_int(-(a->int_value), &front_seg); 
}

// Comparison Functions

static __inline__ value * int_eq(value * a, value * b)
{
  if (a->int_value == b->int_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);
}

static __inline__ value * int_lt(value * a, value * b)
{
  if (a->int_value < b->int_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);

  return NULL;
}

static __inline__ value * int_gt(value * a, value * b)
{
  if (a->int_value > b->int_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);
}

// Floating point Functions

// Arithmetic Functions

static __inline__ value * float_add(value * a, value * b)
{
  return alloc_float(a->float_value + b->float_value, &front_seg); 
}

static __inline__ value * float_sub(value * a, value * b)
{
  return alloc_float(a->float_value - b->float_value, &front_seg); 
}

static __inline__ value * float_mul(value * a, value * b)
{
  return alloc_float(a->float_value * b->float_value, &front_seg); 
}

static __inline__ value * float_div(value * a, value * b)
{
  return alloc_float(a->float_value / b->float_value, &front_seg); 
}

static __inline__ value * float_neg(value * a)
{
  return alloc_float(-(a->float_value), &front_seg); 
}

// Comparison Functions

static __inline__ value * float_eq(value * a, value * b)
{
  if (a->float_value == b->float_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);
}

static __inline__ value * float_lt(value * a, value * b)
{
  if (a->float_value < b->float_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);

  return NULL;
}

static __inline__ value * float_gt(value * a, value * b)
{
  if (a->float_value > b->float_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);
}

// Char functions

//Comparison Functions

static __inline__ value * char_eq(value * a, value * b)
{
  if (a->char_value == b->char_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);
}

static __inline__ value * char_lt(value * a, value * b)
{
  if (a->char_value < b->char_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);

  return NULL;
}

static __inline__ value * char_gt(value * a, value * b)
{
  if (a->char_value > b->char_value)
    return alloc_data("True", 0, &front_seg);
  else
    return alloc_data("False", 0, &front_seg);
}


// Utility functions

static __inline__ int is_constructor(const value *data, char * cons)
{
  assert(data->tag == DATA_VALUE);
  
  return !strcmp(data->data_value.constructor, cons);
}

static __inline__ value * list_cons(value * element, value * list)
{
  assert(list->tag == DATA_VALUE);
  
  value * val = alloc_data("Cons", 2, &front_seg);
  data_set(val, 0, element);
  data_set(val, 1, list);

  return val;
}

static __inline__ value * list_nil(void)
{
  return alloc_data("Nil", 0, &front_seg);
}

#endif /* _INTRINSINC_H_ */

