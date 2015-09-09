#!/bin/bash

# Call this script from the root of the repo

while read -r line;
do
    pair=($line);
    python3 scripts/generate-from-template.py chapters/$1/templates/${pair[0]} chapters/$1/${pair[1]}
done < chapters/$1/templates/templates.list
