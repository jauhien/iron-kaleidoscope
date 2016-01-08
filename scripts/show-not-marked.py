#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
    Script to show not marked pieces of file

    :copyright: (c) 2016 Jauhien Piatlicki
    :license: Apache-2.0 or MIT, see LICENSE-* for details
"""

import argparse
import sys

import snippets

def main():
    parser = argparse.ArgumentParser("Show not marked pieces of FILE")
    parser.add_argument('file_in', help='input FILE')
    args = parser.parse_args()
    file_in = []
    with open(args.file_in) as f:
        file_in = f.readlines()
    file_snippets = snippets.parse_source(file_in)
    try:
        for line in file_snippets["not-marked"]:
            print(line)
    except KeyError:
        pass

if __name__ == "__main__":
    sys.exit(main())
