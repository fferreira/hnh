#ifndef __RUNTIME_H__
#define __RUNTIME_H__

enum types {
  INT_VALUE,
  CHAR_VALUE,
  FLOAT_VALUE,
  //  STRING_VALUE, // not supported right now
  TUPLE_VALUE,
  DATA_VALUE
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
    
    
  };
} value;


typedef struct {
  void * buffer;
  void * curr;
  void * end;
} memory_buffer;

#endif//__RUNTIME_H__
