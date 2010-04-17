#ifndef __CONFIG_H__
#define __CONFIG_H__
#include "runtime.h"

#include <stdint.h>

#define BYTE uint8_t

/* #define MAX_GC_OBJS (2000) */
#define MAX_GC_OBJS (65536)

#define MB (1024 * 1024)

#define MAIN_MEMORY_SEGMENT_SIZE (MAX_GC_OBJS * sizeof(value))
/* #define MAIN_MEMORY_SEGMENT_SIZE (16 * MB) */




#endif /* __CONFIG_H__ */
