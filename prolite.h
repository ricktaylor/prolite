

#if defined(_MSC_VER)
typedef __int32 int32_t;
typedef unsigned __int32 uint32_t;
typedef __int64 int64_t;
typedef unsigned __int64 uint64_t;
#define INT32_MAX _I32_MAX
#define INT64_MAX _I64_MAX

#ifdef  _WIN64
typedef __int64 ssize_t;
#else
typedef unsigned int ssize_t;
#endif
#endif
