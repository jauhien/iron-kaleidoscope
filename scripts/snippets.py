#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""
    Module to work with marked code snippets

    :copyright: (c) 2015-2016 Jauhien Piatlicki
    :license: Apache-2.0 or MIT, see LICENSE-* for details
"""

import re

def parse_source(source):
    snippets = {}
    active_snippets = set()
    for line in source:
        if line[0:3] == "//<":
            active_snippets.update(line[3:].split())
        elif line[0:3] == "//>":
            active_snippets = active_snippets.difference(set(line[3:].split()))
        else:
            join = False
            w = False
            if line[0:5] == "/*j*/":
                line = line[5:]
                join = True
            elif line[0:6] == "/*jw*/":
                line = line[6:]
                join = True
                w = True
            if join:
                line = line.lstrip()
                if w:
                    line = " " + line

            if not active_snippets:
                try:
                    snippets["not-marked"].append(line)
                except KeyError:
                    snippets["not-marked"] = [line]
            else:
                for snippet in active_snippets:
                    try:
                        snippets[snippet].append(line)
                    except KeyError:
                        snippets[snippet] = [line]

                    if join:
                        try:
                            snippets[snippet][-2] = snippets[snippet][-2].rstrip('\n')
                        except IndexError:
                            pass

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

def process(text):
    snippets = Snippets()
    result = []
    regexp = re.compile('<<<(.*):(.*)>>>')
    for line in text:
        match = regexp.match(line)
        if match:
            fname = match.group(1)
            name = match.group(2)
            result.extend(snippets.get(fname, name))
        else:
            result.append(line)
    return result
