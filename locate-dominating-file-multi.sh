# function body template to replace locate-dominating-file
#
# Expanded by `tramp-expand-script'; percent characters need to be doubled

TEST="${1%%/}"  # remove trailing slash, e.g. from dired
shift

# it is possible to receive an empty filename, but then we return no results
[ -z "$TEST" ] && echo "()" && return

# if it's not a directory, will start testing from the containing directory
if [ ! -d "$(%r -f "$TEST")" ]; then
    TEST="$(dirname "$TEST")"
fi

FOUND=""
echo "("
while
    # print files found:
    if [ -d "$TEST" ]; then
        for NAME in "$@"; do
    	    if [ -e "$TEST/$NAME" ]; then
                %k "$TEST/$NAME"
                FOUND=1
    	    fi
        done
    fi
    # determine whether to continue:
    [ -z "$FOUND" ] && [ "$TEST" != "/" ]
do
    TEST=$(dirname "$TEST")
done
echo ")"
