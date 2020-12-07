#!/usr/bin/env bash

CFLAGS="-O3"
ZIGFLAGS="--release-fast"

has() {
    type -p "$1" > /dev/null
}

run() {
    local exe output answer
    case "$1" in
        *.c)
            exe="${1%.c}"
            if has "cc"; then
                cc $CFLAGS -o "$exe" "$1" > /dev/null 2> /dev/null || {
                    printf "\e[31mcc failed on %s!\n\e[m" "$1"
                    return
                }
            else
                printf "\e[31mCan't run a C compiler\n\e[m"
                return
            fi
            ;;

        *.hs)
            exe="${1%.hs}"
            if has "ghc"; then
                ghc --make "$1" > /dev/null 2> /dev/null || {
                    printf "\e[31mghc failed on %s!\n\e[m" "$1"
                    return
                }
            else
                printf "\e[31mCan't run ghc\n\e[m"
                return
            fi
            ;;

        *.zig)
            exe="${1%.zig}"
            if has "zig"; then
                zig build-exe $ZIGFLAGS "$1" > /dev/null 2> /dev/null || {
                    printf "\e[31mzig failed on %s!\n\e[m" "$1"
                    return
                }
            else
                printf "\e[31mCan't run zig\n\e[m"
                return
            fi
            ;;

        *.raku)
            exe="$1"
            if has "raku"; then
                true
            else
                printf "\e[31mThere is no raku interpretor\n\e[m"
                return
            fi
            ;;

        *)
            #printf "\e[33mDon't know how to run %s\n\e[m" "$1"
            return
            ;;
    esac

    printf "Running %s..." "$1"
    output="$("./$exe" < input 2> /dev/null)" || {
        printf " \e[31mFailed!\n\e[m"
        return
    }
    answer="$(< answers)" || {
        printf " \e[31mFailed to get answers from file!\n\e[m"
        return
    }
    if [[ "$output" == "$answer" ]]; then
        printf " \e[32mSuccess!\n\e[m"
    else
        printf " \e[31mFailed!\n\e[m"
        printf "  \e[31mExpected:\n\e[m%s\n  \e[31mGot:\n\e[m%s\n" "$answer" "$output"
        return
    fi
}

main() {
    for dir in "$(dirname "$0")"/20??/??; do
        pushd "$dir" > /dev/null
        for file in *; do
            run "$file"
        done
        popd > /dev/null
    done
}

main "$@"
