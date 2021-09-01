AC_DEFUN([PROLITE_PROG_CC],
[
	AC_BEFORE([$0],[AC_PROG_CC])
	
	# Call AC_PROG_CC_C99
	AC_PROG_CC_C99

	# Restore unmodified CFLAGS
	CFLAGS=$ac_save_CFLAGS
	
	# Add the --enable-debug arg
	AC_ARG_ENABLE([debug],AS_HELP_STRING([--enable-debug],[Create a debug build]))

	AS_IF([test -z "$CPPFLAGS" -a -z "$CFLAGS" -a -z "$LDFLAGS"],
	[
		AS_IF([test "x$enable_debug" = "xyes"],
		[
			CFLAGS="-g3 -O0"
		],
		[
			CPPFLAGS="-D_FORTIFY_SOURCE=2"
			CFLAGS="-O3 -fstack-protector-all"
			LDFLAGS="-O3 -Wl,-z,relro,-z,now"
		])
	])
	AS_IF([test "x$enable_debug" != "xyes"],[CPPFLAGS="$CPPFLAGS -DNDEBUG"])
	
	# Turn on all warnings and errors
	CFLAGS="$CFLAGS -Wall -Werror -pipe -fvisibility=hidden -Wstrict-prototypes -Wformat-security"

	# Add the coverage options
	AC_ARG_ENABLE([coverage],AS_HELP_STRING([--enable-coverage],[Create a code-coverage build]))
	AS_IF([test "x$enable_coverage" = "xyes"],
	[
		CFLAGS="$CFLAGS --coverage -fprofile-abs-path"
		LDFLAGS="$LDFLAGS --coverage"
	])

	# Add the "link-time-optimisation" options
	AC_ARG_ENABLE([lto],AS_HELP_STRING([--enable-lto],[Enable Link-time optimisation]))
	AS_IF([test "x$enable_lto" = "xyes"],
	[
		CFLAGS="$CFLAGS -flto"
		LDFLAGS="$LDFLAGS -flto"
	])
])
