#!/usr/bin/env python

from __future__ import unicode_literals

from functools import cmp_to_key
import json
import sys

def get_config(query):
    config = {}
    def connected(output):
        return query[output]['connected']
    def index(output):
        return query[output]['index']
    def cmp(x, y):
        if connected(x) and not connected(y):
            return -1
        if not connected(x) and connected(y):
            return 1
        return index(x) - index(y)
    outputs = sorted(query.keys(), key=cmp_to_key(cmp))
    last_config = None
    for output in outputs:
        if not connected(output):
            config[output] = None
            continue
        preferred = query[output]['preferred']
        width = preferred['width']
        height = preferred['height']
        rate = preferred['rate']
        if last_config:
            x = last_config['x'] + width
            y = last_config['y'] - height
        else:
            x = y = 0
        last_config = config[output] = {
            'x': x,
            'y': y,
            'width': width,
            'height': height,
            'rate': rate,
        }
    return config

def main():
    query = json.load(sys.stdin)
    config = get_config(query)
    json.dump(config, sys.stdout)

if __name__ == '__main__':
    main()
