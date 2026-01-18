DIR="$1"
shift
if [ ! -d "$DIR" ]; then
    echo nil
else
    DIR="$(realpath "$DIR")"
    cd "$DIR"
    echo \(
    for FILE in "$@"; do
        if [ -r "$FILE" ] && [ -f "$FILE" ] && [ ! -d "$FILE" ]; then
            %k "$DIR/$FILE"; printf "\n"
        fi
    done
    echo \)
fi
