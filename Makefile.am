
ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libprolite.la

libprolite_la_SOURCES = \
	lib/interpreter.c \
	lib/prolite.c \
	lib/strings.c \
	lib/context.c \
	lib/parser.c \
	lib/exception.c \
	lib/clause.c \
	lib/module.c \
	lib/stack.c
				
libprolite_la_LDFLAGS = -pthread

#check_PROGRAMS = test_libprolite

#test_libprolite_SOURCES = 

#test_libprolite_LDADD = libprolite.la -lcunit
#test_libprolite_LDFLAGS = -pthread -no-install -static

bin_PROGRAMS = prolite 

prolite_SOURCES = \
	src/main.c
	
prolite_LDADD = libprolite.la
prolite_LDFLAGS = -pthread -static
