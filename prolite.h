
#if defined(_MSC_VER)
#ifdef  _WIN64
typedef __int64 ssize_t;
#define _INTPTR 2
#else
typedef unsigned int ssize_t;
#define _INTPTR 1
#endif
#endif

#include <stdint.h>
