
ACLOCAL_AMFLAGS = -I m4

AM_DISTCHECK_CONFIGURE_FLAGS = --disable-tests

AM_CPPFLAGS = -DVERSION=\"$(VERSION)\"
if ENABLE_TESTS
AM_CPPFLAGS += -DENABLE_TESTS
endif

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
	lib/operators.c \
	lib/char_conv.c \
	lib/consult.c \
	lib/exceptions.c \
	lib/write_term.c \
	lib/compile_expr.c

libprolite_la_LDFLAGS = -version-number $(shell echo $(VERSION) | grep -oP '^\d+.\d+.\d+' | sed 's/\./:/g')
libprolite_la_LIBADD = -lm

bin_PROGRAMS = prolite

prolite_SOURCES = \
	src/settings.c \
	src/fs_resolver.c \
	src/main.c

prolite_LDADD = libprolite.la
prolite_LDFLAGS = -static

if ENABLE_TESTS
check_PROGRAMS = \
	test_btree

TESTLDADD = libprolite.la
TESTLDFLAGS = -no-install -static

nodist_test_btree_SOURCES = \
	test/internal/btree_tests.c

test_btree_LDADD = $(TESTLDADD)
test_btree_LDFLAGS = $(TESTLDFLAGS)

endif # ENABLE_TESTS

dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version
