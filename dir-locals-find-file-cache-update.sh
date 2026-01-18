FILE="$1"
shift
NAMES="$1"
shift
STAT_FORMAT="%%Y"

# If FILE doesn't exist yet, find the first ancestor that does
TEST="$FILE"
if [ -e "$FILE" ]; then
    FILE="$(realpath "$FILE")"
    STARTING="$FILE"
else
    STARTING=""
    while [ -z "$STARTING" ] && [ ! -z "$TEST" ]; do
        if [ -d "$TEST" ]; then
            STARTING="$TEST"
        else
            if [ "$TEST" = "/" ]; then
                TEST=""
            else
                TEST="${TEST%%/*}"
                if [ -z "$TEST" ]; then
                    TEST="/"
                fi
            fi
        fi
    done
fi

# If we haven't found an ancestor, that's an error
if [ -z "$STARTING" ]; then
    echo nil
else
    TEST="$(realpath "$STARTING")"

    # Make sure we're looking directories
    if [ ! -d "$TEST" ]; then
        TEST="$(dirname "$TEST")"
    fi

    # Start the plist with the real filename
    echo "("
    printf ":file "; %k "$FILE"; printf "\n"

    # Walk up the directory structure looking for the search files
    FOUND=""
    while [ ! -z "$TEST" ] && [ -z "$FOUND" ]; do
        for NAME in $NAMES; do
            if [ -f "$TEST/$NAME" ]; then
                DOMINATING_DIR="$TEST"
                MTIME="$(stat -c "$STAT_FORMAT" "$TEST/$NAME")"
                FOUND="$FOUND ( \"$NAME\" . $MTIME ) "
            fi
        done
        if [ -z "$FOUND" ]; then
            if [ "$TEST" = "/" ]; then
                TEST=""
            else
                TEST="${TEST%%/*}"
                if [ -z "$TEST" ]; then
                    TEST="/"
                fi
            fi
        fi
    done

    # Add found files to the plist
    if [ ! -z "$FOUND" ]; then
        printf ":locals ("; %k "$DOMINATING_DIR/"; echo " $FOUND)"
    fi

    # Test cached dirs for updated mtime
    DOMINATING_DIR_LEN=$(expr length "$DOMINATING_DIR")
    FOUND_CACHEDIR=""
    FOUND_CACHEDIR_LEN=0
    for CACHEDIR in "$@"; do
        CACHEDIR_LEN=$(expr length "$CACHEDIR")

        if [ -d "$CACHEDIR" ] \
          && [ "$CACHEDIR_LEN" -gt "$FOUND_CACHEDIR_LEN" ] \
          && [ "${FILE#$CACHEDIR}" != "$FILE" ]; then
            FOUND_CACHEDIR="$CACHEDIR"
            FOUND_CACHEDIR_LEN=$CACHEDIR_LEN
        fi
    done

    # Add updated cachedirs to plist
    if [ ! -z "$FOUND_CACHEDIR" ]; then
        echo ":cache ( \"$FOUND_CACHEDIR\" "
        for NAME in $NAMES; do
            if [ -f "$FOUND_CACHEDIR/$NAME" ]; then
                MTIME="$(stat -c "$STAT_FORMAT" "$FOUND_CACHEDIR/$NAME")"
                echo "( \"$NAME\" . $MTIME ) "
            fi
        done
        echo ")"
    fi

    echo ")"
fi
