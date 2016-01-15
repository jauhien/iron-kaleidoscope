#!/bin/sh

# Call this script from the root of the repo
for i in src/*.rs translator/src/*.rs grammar.ebnf;
do
    echo
    echo "********************************************************************************"
    echo $i
    echo "********************************************************************************"
    echo
    python3 scripts/show-not-marked.py $i
done
