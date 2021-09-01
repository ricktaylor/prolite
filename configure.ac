AC_PREREQ(2.61)

AC_INIT(libprolite,m4_esyscmd([build-aux/git-version-gen.sh]))
AC_CONFIG_SRCDIR([src/main.c])
AC_CONFIG_HEADERS([autoheader_dummy.h])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AC_CONFIG_FILES([Makefile])

AM_INIT_AUTOMAKE([foreign 1.10 no-define subdir-objects])

# Use the latest silent rules if possible
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

AM_MAINTAINER_MODE([enable])
AC_CANONICAL_HOST

PROLITE_PROG_CC

m4_ifdef([LT_PREREQ],,[AC_MSG_ERROR([Need libtool version 2.2.6 or later])])
LT_PREREQ([2.2.6])
LT_INIT

# Disable test when requested
AC_ARG_ENABLE([tests],AS_HELP_STRING([--enable-tests],[Enable unit tests]))
AM_CONDITIONAL([ENABLE_TESTS], [test "x$enable_tests" != "xno"])

AC_OUTPUT
