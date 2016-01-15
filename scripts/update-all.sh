#!/bin/bash

# Call this script from the root of the repo
cargo update

for i in `cat chapters/chapters.list`;
do
    cd chapters/$i
    cargo update
    cd -
done

cd translator
cargo update
cd -
