#!/bin/bash

# Call this script from the root of the repo
cargo build

for i in `cat chapters/chapters.list`;
do
    cd chapters/$i
    cargo build
    cd -
done

cd translator
cargo build
cd -
