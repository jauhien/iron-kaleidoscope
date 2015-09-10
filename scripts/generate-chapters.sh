#!/bin/sh

# Call this script from the root of the repo
for i in 0 1;
do
    scripts/generate-chapter.sh $i
done
