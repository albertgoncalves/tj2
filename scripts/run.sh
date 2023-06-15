#!/usr/bin/env bash

set -eu

for x in bin build; do
    if [ ! -d "$WD/$x" ]; then
        mkdir "$WD/$x"
    fi
done

flags_hs=(
    "-fdiagnostics-color=always"
    "-i$WD/src"
    "-outputdir $WD/build"
    -Wall
    -Wcompat
    -Werror
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmonomorphism-restriction
    -Wpartial-fields
    -Wredundant-constraints
    -Wunused-packages
    -Wunused-type-patterns
)

for x in "$WD/src"/*.hs; do
    (
        hlint "$x"
        ormolu -i --no-cabal "$x"
    ) &
done

for _ in $(jobs -p); do
    wait -n
done

ghc "${flags_hs[@]}" -o "$WD/bin/main" "$WD/src/Main.hs"
"$WD/bin/main" "$1"
