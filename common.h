#ifndef tz_common_h
#define tz_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define DEBUG_PRINT_CODE
#define DEBUG_TRACE_EXECUTION

#define DEBUG_STRESS_GC
//#define DEBUG_LOG_GC //p.504 when this is enabled, it prints information to the console when it does something with dynamic memory

#define UINT8_COUNT ( UINT8_MAX + 1 )

#endif
