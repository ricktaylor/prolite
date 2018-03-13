
ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libprolite.la

libprolite_la_SOURCES = \
	clause.c \
	interpreter.c \
	prolite.c \
	strings.c \
	context.c \
	parser.c \
	stack.c \
	throw.c
				
libprolite_la_LDFLAGS = -pthread

#check_PROGRAMS = prolite test_libprolite

#test_libprolite_SOURCES = 

#test_libprolite_LDADD = libprolite.la -lcunit
#test_libprolite_LDFLAGS = -pthread -no-install -static

#prolite_SOURCES = \
#	test/server/main.c
	
#prolite_LDADD = libprolite.la
#prolite_LDFLAGS = -pthread -no-install -static
