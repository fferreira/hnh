#include "runtime.h"
#include "config.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Error management

void fail(char * s)
{
  printf ("%s\n", s);
  exit(1);
}

// Memory management

static memory_buffer alloc_memory_seg(void)
{
  void * ptr = malloc(MAIN_MEMORY_SEGMENT_SIZE);
  memory_buffer buff;

  if (ptr) {
    buff.buffer = ptr;
    buff.curr = ptr;
    buff.end = (value *)((char *) ptr + MAIN_MEMORY_SEGMENT_SIZE);
  } else 
    fail("Unable to allocate memory");

  return buff;
}

static void free_memory_seg(void * ptr)
{
  if (ptr != NULL) free(ptr);
}

memory_buffer current_seg, back_seg;

void init_memory(void)
{
  current_seg = alloc_memory_seg();
  back_seg = alloc_memory_seg();
}

void swap_segs(void)
{
  memory_buffer bkp = back_seg;
  back_seg = current_seg;
  current_seg = bkp;
}

static void * alloc_mem(size_t size)
{
  BYTE * ptr = (BYTE *)current_seg.curr;

  if ((ptr + size) > (BYTE *)current_seg.end)
    fail("Out of memory");
  
  current_seg.curr = (void *)(ptr + size);

  return ptr;
}

value * alloc_int(int n)
{
  value * val = (value *) alloc_mem(sizeof(value));
  val->tag = INT_VALUE;
  val->int_value = n;
  return val;
}

value * alloc_tuple(int size)
{
  value * val = (value *) alloc_mem(sizeof(value));
  value ** fields = (value **) alloc_mem(size * sizeof(value *));
  val->tag = TUPLE_VALUE;
  val->tuple_value.num_of_fields = size;
  val->tuple_value.fields = fields;
  return val;
}

value * alloc_data(const char * con, int size)
{
  value * val = (value *) alloc_mem(sizeof(value));
  char * name = (char *) alloc_mem(strlen(con) * sizeof(char));
  value ** fields = (value **) alloc_mem(size * sizeof(value *));

  strcpy(name, con);
  val->tag = DATA_VALUE;
  val->data_value.constructor = name;
  val->data_value.fields = fields;
  return val;
}



// Tuple and datatype access functions

value * tup_get(const value * val, int n)
{
  if ((val->tag == TUPLE_VALUE) && (val->tuple_value.num_of_fields > n)) {
      return val->tuple_value.fields[n];
    } else {
    fail ("Invalid tuple access");
  }
  
  return NULL;
}

void tup_set(value * val, int n, value * v)
{
  if ((val->tag == TUPLE_VALUE) && (val->tuple_value.num_of_fields > n)) {
    val->tuple_value.fields[n] = v;
  } else {
    fail ("Invalid tuple access");
  }

}

value * data_get(value * val, int n)
{
  if ((val->tag == DATA_VALUE) && (val->data_value.num_of_fields > n)) {
    return val->data_value.fields[n];
  } else {
    fail ("Invalid tuple access");
  }
  return NULL;
}

void data_set(value * val, int n, value * v)
{
  if ((val->tag == DATA_VALUE) && (val->data_value.num_of_fields > n)) {
    val->data_value.fields[n] = v;
  } else {
    fail ("Invalid tuple access");
  }
}


// Main function

int main(int argc, char *argv[])
{
  
  return 0;
}


