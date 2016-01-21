#!/usr/bin/env python

from subprocess import check_output
import subprocess
from functools import cmp_to_key
import json

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
    x = y = 0
    for output in outputs:
        if not connected(output):
            config[output] = None
            continue
        preferred = query[output]['preferred']
        width = preferred['width']
        height = preferred['height']
        rate = preferred['rate']
        config[output] = {
            'x': x,
            'y': y,
            'width': width,
            'height': height,
            'rate': rate,
        }
        x += width
        y += height
    return config

def multi_head_setup(config):
    cmd = ['xrandr']
    for k in config.keys():
        v = config[k]
        cmd += ['--output', k]
        if v:
            cmd += ['--pos', '{x}x{y}'.format(**v)]
            cmd += ['--mode', '{width}x{height}'.format(**v)]
            cmd += ['--rate', '{rate}'.format(**v)]
        else:
            cmd += ['--off']
    subprocess.call(cmd)

def main():
    output = check_output(['xrandr2json'], universal_newlines=True)
    query = json.loads(output)
    config = get_config(query)
    multi_head_setup(config)

if __name__ == '__main__':
    main()
