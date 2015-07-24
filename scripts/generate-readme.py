#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
    Script to generate README with pieces of source files

    :copyright: (c) 2015 Jauhien Piatlicki
    :license: Apache-2.0 or MIT, see LICENSE-* for details
"""

import argparse
import re
import sys

def parse_source(source):
    snippets = {}
    active_snippets = set()
    for line in source:
        if line[0:3] == "//<":
            active_snippets.update(line[3:].split())
        elif line[0:3] == "//>":
            active_snippets = active_snippets.difference(set(line[3:].split()))
        else:
            for snippet in active_snippets:
                try:
                    snippets[snippet].append(line)
                except KeyError:
                    snippets[snippet] = [line]
    return snippets

def parse_source_file(fname):
    source = []
    with open(fname) as f:
        source = f.readlines()
    return parse_source(source)

class Snippets(object):
    __slots__ = ['snippets']

    def __init__(self):
        self.snippets = {}

    def get(self, fname, name):
        snippets = {}
        try:
            snippets = self.snippets[fname]
        except KeyError:
            snippets = parse_source_file(fname)
            self.snippets[fname] = snippets
        return snippets[name]

def process_readme(readme):
    snippets = Snippets()
    result = []
    regexp = re.compile('<<<(.*):(.*)>>>')
    for line in readme:
        match = regexp.match(line)
        if match:
            fname = match.group(1)
            name = match.group(2)
            result.extend(snippets.get(fname, name))
        else:
            result.append(line)
    return result

def main():
    parser = argparse.ArgumentParser("Generate README with code snippets")
    parser.add_argument('readme_in', help='README template')
    parser.add_argument('readme_out', help='README to be generated')
    args = parser.parse_args()
    readme_in = []
    with open(args.readme_in) as f:
        readme_in = f.readlines()
    readme = process_readme(readme_in)
    with open(args.readme_out, 'w') as f:
        f.write("".join(readme))

if __name__ == "__main__":
    sys.exit(main())
