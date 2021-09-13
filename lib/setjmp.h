#ifndef PROLITE_SETJMP_H
#define PROLITE_SETJMP_H

#include <setjmp.h>

#if defined(__linux__)
#if defined(setjmp)
#undef setjmp
#endif
#define setjmp(n) sigsetjmp(n,1)
#define longjmp(m,n) siglongjmp(m,n)
#endif

#endif // PROLITE_SETJMP_H
