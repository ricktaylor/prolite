#!/bin/sh

LF='
'

# First see if there is a .tarball-version file (included in release tarballs),
# then try git-describe, then default.
if test -f .tarball-version
then
	VN=$(cat .tarball-version) || exit 1
elif test -d ${GIT_DIR:-.git} -o -f .git &&
	VN=$(git describe --match "v[0-9]*" HEAD 2>/dev/null) &&
	case "$VN" in
	*$LF*) (exit 1) ;;
	v[0-9]*)
		git update-index -q --refresh
		test -z "$(git diff-index --name-only HEAD --)" ||
		VN="$VN-dirty" ;;
	esac
then
	VN=$(echo "$VN" | sed -e 's/\(.*\)-g/\1-dev-/');
	VN=$(expr "$VN" : v*'\(.*\)')

else
	VN="0.0.1-no-tag-found"
fi

# Omit the trailing newline, so that m4_esyscmd can use the result directly.
echo "$VN" | tr -d "$LF"
