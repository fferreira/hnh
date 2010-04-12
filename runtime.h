#ifndef __RUNTIME_H__
#define __RUNTIME_H__

enum types {
  INT_VALUE,
  CHAR_VALUE,
  FLOAT_VALUE,
  //  STRING_VALUE, // not supported right now
  TUPLE_VALUE,
  DATA_VALUE,
  FUNCTION_VALUE
};


typedef struct _value {
  enum types tag; // the type of the vale
  union {
    int int_value;
    char char_value;
    double float_value;
    struct {
      unsigned int num_of_fields;
      struct _value ** fields;
    } tuple_value;

    struct {
      char * constructor;
      unsigned int num_of_fields;
      struct _value ** fields;
    } data_value;
    
    void * function;  //TODO add the correct type
  };
} value;


typedef struct {
  void * buffer;
  void * curr;
  void * end;
} memory_buffer;

// memory management

void init_memory(void);
void swap_segs(void);
value * alloc_int(int n);
value * alloc_tuple(int size);
value * alloc_data(const char * con, int size);

// tuple and datatyupe access functions

value * tup_get(const value * val, int n);
void tup_set(value * val, int n, value * v);
value * data_get(value * val, int n);
void data_set(value * val, int n, value * v);

// printing function

void print_value(const value * val);

// Predefined names

#define RES (result)

// Program result declaration

extern struct _value * RES;

// Final Continuation

void halt_continuation(void);

// Generated Functions

void init_fun(void);
void HNH_main(void);

#endif//__RUNTIME_H__
