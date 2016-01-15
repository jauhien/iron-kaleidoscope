#!/bin/bash

# Call this script from the root of the repo
cargo clean

for i in `cat chapters/chapters.list`;
do
    cd chapters/$i
    cargo clean
    cd -
done

cd translator
cargo clean
cd -
