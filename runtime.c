#include "runtime.h"
#include "config.h"
#include "gc.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

// Error management

void fail(char * s)
{
  printf ("%s\n", s);
  exit(1);
}

// Memory management

memory_buffer front_seg, back_seg, perm_seg;

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

static void free_memory_seg(memory_buffer * ptr)
{
  if (ptr->buffer != NULL) {
    free(ptr->buffer);
    ptr->buffer = ptr->curr = ptr-> end = NULL;
  }
}

void clear_back_seg(void)
{
  memset(back_seg.buffer, 0, MAIN_MEMORY_SEGMENT_SIZE);
  back_seg.curr = back_seg.buffer;
}

void init_memory(void)
{
  front_seg = alloc_memory_seg();
  back_seg = alloc_memory_seg();
  perm_seg = alloc_memory_seg();
}

void swap_segs(void)
{
  memory_buffer bkp = back_seg;
  back_seg = front_seg;
  front_seg = bkp;
  back_seg.curr = back_seg.buffer;
}

static void * alloc_mem(size_t size, memory_buffer * seg)
{
  BYTE * ptr = (BYTE *)seg->curr;

  if ((ptr + size) > (BYTE *)seg->end)
    fail("Out of memory");
  
  seg->curr = (void *)(ptr + size);

  return ptr;
}

value * alloc_int(int n, memory_buffer * seg)
{
  value * val = (value *) alloc_mem(sizeof(value), seg);
  val->tag = INT_VALUE;
  val->int_value = n;
  return val;
}

value * alloc_char(char c, memory_buffer * seg)
{
  value * val = (value *) alloc_mem(sizeof(value), seg);
  val->tag = CHAR_VALUE;
  val->char_value = c;
  return val;
}

value * alloc_float(float f, memory_buffer * seg)
{
  value * val = (value *) alloc_mem(sizeof(value), seg);
  val->tag = FLOAT_VALUE;
  val->float_value = f;
  return val;
}


value * alloc_tuple(int size, memory_buffer * seg)
{
  value * val = (value *) alloc_mem(sizeof(value), seg);
  value ** fields = (value **) alloc_mem(size * sizeof(value *), seg);
  val->tag = TUPLE_VALUE;
  val->tuple_value.num_of_fields = size;
  val->tuple_value.fields = fields;
  return val;
}

value * alloc_data(const char * con, int size, memory_buffer * seg)
{
  value * val = (value *) alloc_mem(sizeof(value), seg);
  char * name = (char *) alloc_mem((1+strlen(con)) * sizeof(char), seg);
  value ** fields = NULL;

  strcpy(name, con);
  val->tag = DATA_VALUE;
  val->data_value.constructor = name;

  if (size!=0) {
    fields = (value **) alloc_mem(size * sizeof(value *), seg);
  }

  val->data_value.num_of_fields = size;
  val->data_value.fields = fields;  
  return val;
}

value * alloc_function(fun_ptr fun, memory_buffer * seg)
{
  value * val = (value *) alloc_mem(sizeof(value), seg);
  val->tag = FUNCTION_VALUE;
  val->function = fun;

  return val;
}

static int is_seg_ptr(void * ptr, memory_buffer * seg)
{
  return ((ptr >= seg->buffer) && (ptr < seg->end));
}

int is_perm_ptr(void * ptr)
{
  return is_seg_ptr(ptr, &perm_seg);
}

int is_front_ptr(void * ptr)
{
  return is_seg_ptr(ptr, &front_seg);
}

int is_back_ptr(void * ptr)
{
  return is_seg_ptr(ptr, &back_seg);
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

value * data_get(const value * val, int n)
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

// printing functions

static void print_string(value * val)
{
  value * c;
  assert(val->tag == DATA_VALUE);
  if(strcmp("Nil", val->data_value.constructor) == 0) {
    printf (" ");
    return;
  }
  c = data_get(val, 0);
  assert(c->tag == CHAR_VALUE);
  printf ("%c",c->char_value);
  print_string(data_get(val, 1));
  
}

static void print_list(value * val)
{
  value * head, * tail;
  assert(val->tag == DATA_VALUE);
  if(strcmp("Nil", val->data_value.constructor) == 0) {
      return;
  }
  head = data_get(val, 0);
  tail = data_get(val, 1);

  print_value(head);
  if(strcmp("Nil", tail->data_value.constructor) != 0)
    printf (", ");
  print_list(data_get(val, 1));

}

void print_value(value * val)
{
  int i;
  switch (val->tag) {
  case INT_VALUE:
    printf("%d ", val->int_value);
    break;
  case CHAR_VALUE:
    printf("'%c' ", val->char_value);
    break;
  case FLOAT_VALUE:
    printf("%f", val->float_value);
    break;
  case TUPLE_VALUE:
    printf ("(");
    for(i = 0 ; i < val->tuple_value.num_of_fields ; i++) {
      print_value(tup_get(val, i));
      if (i != (val->tuple_value.num_of_fields - 1)) printf(", ");
    }
    printf(") ");
    break;
  case DATA_VALUE:
    if (strcmp("Cons", val->data_value.constructor) == 0) {
      // technically not necessary (i == 2 always)
      if ((i >= 1) && (data_get(val, 0)->tag == CHAR_VALUE)) {
	print_string(val);
	break;
      } else {
	printf ("[");
	print_list(val);
	printf("] ");
	break;
      }
    }
    // print datatype
    printf("%s ", val->data_value.constructor);
    for(i = 0 ; i < val->data_value.num_of_fields ; i++) {
      print_value(data_get(val, i));
    }
    break;
  case FUNCTION_VALUE:
  default:
    printf ("unimplemented "); 
  };
}

// Program Result

static value final_result;
value * RES;

// Final Continuation
void halt_continuation(value * v)
{
  print_value(v);
  printf ("\n");
  exit(0);
}

// Main function

int main(int argc, char *argv[])
{
  init_memory();

  RES = &final_result;
  RES->tag = INT_VALUE;
  RES->int_value = 0;

  init_fun();

  // Trampoline

  call_k next;
  next.fun = HNH_main;
  next.params = NULL; //No params for now

  while(1) {
    next = (next.fun) (next.params);
    
    next = gc(next);
  }

  return 0;
}



