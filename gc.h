#ifndef _GC_H_
#define _GC_H_

#include "runtime.h"

call_k gc(call_k roots);
int gc_is_needed(void);



#endif /* _GC_H_ */
