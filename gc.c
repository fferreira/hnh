#include "gc.h"
#include "runtime.h"
#include "config.h"

#include <assert.h>

extern int num_fun_root;
extern value * fun_root[];

value * seen_obj[MAX_GC_OBJS];
value * new_obj[MAX_GC_OBJS];
int last_seen = 0;

#define NOT_FOUND (-1)

void init_seen(void)
{
  last_seen = 0;
}

int find_seen(value * ptr)
{
  int i;
  for(i = 0 ; i < last_seen ; i++) {
    if (ptr == seen_obj[i])
      return i;
  }
  return NOT_FOUND;
}

void add_seen(value * old, value * new)
{
  seen_obj[last_seen] = old;
  new_obj[last_seen] = new;

  last_seen ++;
  
  if (last_seen >= MAX_GC_OBJS)
    fail("BUG: add dynamic allocation to gc seen");
}

value * copy(value * val)
{
  value * ret = NULL;
  int i;
  int seen = find_seen(val);

  if(is_perm_ptr(val)) 
    return val; // permanent memory segment is not gc'd

  assert(is_back_ptr(val));

  if(seen != NOT_FOUND)
    return new_obj[seen];

  switch (val->tag)
  {
  case INT_VALUE:
    ret = alloc_int(val->int_value, &front_seg);
    add_seen(val, ret);
    break;

  case CHAR_VALUE:
    ret = alloc_char(val->char_value, &front_seg);
    add_seen(val, ret);
    break;

  case FLOAT_VALUE:
    ret = alloc_float(val->float_value, &front_seg);
    add_seen(val, ret);
    break;


 case FUNCTION_VALUE:
   ret = alloc_function(val->function, &front_seg);
   add_seen(val, ret); // usually functions are in the perm seg
   break;

  case TUPLE_VALUE:
    ret = alloc_tuple(val->tuple_value.num_of_fields, &front_seg);
    add_seen(val, ret);

    for (i = 0 ; i < val->tuple_value.num_of_fields ; i++)
      {
	tup_set(ret, i, copy(tup_get(val, i)));
      }
    break;
    
  case DATA_VALUE:
    ret = alloc_data(val->data_value.constructor, val->data_value.num_of_fields, &front_seg);
    add_seen(val, ret);

    for (i = 0 ; i < val->data_value.num_of_fields ; i++)
      {
	data_set(ret, i, copy(data_get(val, i)));
      }
    break;
 
  case UNINITIALIZED:
    fail ("GC Error: tried to copy an invalid object\n");
    break;

  default:
    fail("Unsupported value");
    break;
  }

  assert(is_front_ptr(ret));
  return ret;
}

int gc_is_needed(void)
{
  return ((front_seg.curr - front_seg.buffer) >= (MAIN_MEMORY_SEGMENT_SIZE/2));
}

call_k gc(call_k roots)
{
  call_k new;
  int total = front_seg.end - front_seg.buffer;
  int prev = front_seg.curr - front_seg.buffer;
  int post;
  int i;


  init_seen();
  swap_segs();

  new.fun = roots.fun;
  new.params = copy(roots.params);

  post = front_seg.curr - front_seg.buffer;

  // Not really needed but good for error trapping
  clear_back_seg(); 

  printf ("GC: objects copied: %d, bytes saved: %d\n", last_seen, (prev - post));
  return new;
}
