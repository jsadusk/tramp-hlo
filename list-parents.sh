# function body template for finding the list of parent directories
#
# Expanded by `tramp-expand-script'; percent characters need to be doubled

TEST="${1%%/}"  # remove trailing slash, e.g. from dired

# it is possible to receive an empty filename, but then we return nil
[ -z "$TEST" ] && echo "()" && return

# if it's not a directory, will start testing from the containing directory
if [ ! -d "$(%r -f "$TEST")" ]; then
    TEST="$(dirname "$TEST")"
fi

echo "("
while
    # print the abbreviated dirname
    %k "$TEST" | sed "s|^$HOME|~|"
    # determine whether to continue:
    [ "$TEST" != "/" ]
do TEST=$(dirname "$TEST"); done
echo ")"
