
ACLOCAL_AMFLAGS = -I m4

AM_DISTCHECK_CONFIGURE_FLAGS = --disable-tests

lib_LTLIBRARIES = libprolite.la

proliteincludedir = @includedir@/prolite
#proliteinclude_HEADERS = 

libprolite_la_SOURCES = \
	lib/heap.c \
	lib/btree.c \
	lib/types.c \
	lib/parser.c \
	lib/context.c \
	lib/compile.c \
	lib/predicates.c \
	lib/exceptions.c \
	lib/dump_cfg.c
				
libprolite_la_LDFLAGS = -pthread -version-number $(shell echo $(VERSION) | grep -oP '^\d+.\d+.\d+' | sed 's/\./:/g') 

bin_PROGRAMS = prolite 

prolite_SOURCES = \
	src/consult.c \
	src/main.c
	
prolite_CPPFLAGS = -DVERSION=\"$(VERSION)\"
prolite_LDADD = libprolite.la
prolite_LDFLAGS = -pthread -static

if ENABLE_TESTS
#check_PROGRAMS = \
#	test_libprolite

#TESTLDADD = libprolite.la
#TESTLDFLAGS = -pthread -no-install -static

#nodist_test_libprolite_SOURCES = \
#	test/main.c

#test_libprolite_CFLAGS = -DPACKAGE_NAME=\"$(PACKAGE_NAME)\"
#test_libprolite_LDADD = $(TESTLDADD) -lcunit
#test_libprolite_LDFLAGS = $(TESTLDFLAGS)

endif # ENABLE_TESTS

dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version
