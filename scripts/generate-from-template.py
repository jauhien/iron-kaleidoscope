#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
    Script to generate FILE with pieces of source files

    :copyright: (c) 2015-2016 Jauhien Piatlicki
    :license: Apache-2.0 or MIT, see LICENSE-* for details
"""

import argparse
import sys

import snippets

def main():
    parser = argparse.ArgumentParser("Generate FILE with code snippets")
    parser.add_argument('file_in', help='FILE template')
    parser.add_argument('file_out', help='FILE to be generated')
    args = parser.parse_args()
    file_in = []
    with open(args.file_in) as f:
        file_in = f.readlines()
    text = snippets.process(file_in)
    with open(args.file_out, 'w') as f:
        f.write("".join(text))

if __name__ == "__main__":
    sys.exit(main())
