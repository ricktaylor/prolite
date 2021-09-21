#ifndef EXPORT_H_INCLUDED
#define EXPORT_H_INCLUDED

#if _WIN32
#define PROLITE_EXPORT __declspec(dllexport)
#elif __GNUC__ >= 4
#define PROLITE_EXPORT __attribute__((visibility("default")))
#else
#define PROLITE_EXPORT
#endif

#endif // EXPORT_H_INCLUDED
