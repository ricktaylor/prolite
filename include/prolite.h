#ifndef PROLITE_H_INCLUDED
#define PROLITE_H_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#if __GNUC__ >= 4
#define PROLITE_EXPORT __attribute__((visibility("default")))
#else
#define PROLITE_EXPORT
#endif

#ifdef __cplusplus
}
#endif

#endif // PROLITE_H_INCLUDED
