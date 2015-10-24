#!/usr/bin/env python

# This code is borrowed from: https://github.com/substack/xrandr-parse

from subprocess import check_output
import subprocess
import re
from functools import cmp_to_key

regex = {
    'connected': re.compile('^(\S+) connected (?:(\d+)x(\d+))?'),
    'disconnected': re.compile('^(\S+) disconnected'),
    'mode': re.compile('^\s+(\d+)x(\d+)\s+((?:\d+\.)?\d+)([* ]?)([+ ]?)'),
}

def parse(src):
    lines = src.splitlines()
    query = {}
    last = None
    index = 0

    for line in lines:
        m = regex['connected'].search(line)
        if m:
            index += 1
            query[m.group(1)] = {
                'connected': True,
                'modes': [],
                'index': index,
            }
            if (m.group(2) and m.group(3)):
                query[m.group(1)]['width'] = int(m.group(2))
                query[m.group(1)]['height'] = int(m.group(3))
            last = m.group(1)
            continue
        m = regex['disconnected'].search(line)
        if m:
            index += 1
            query[m.group(1)] = {
                'connected': False,
                'modes': [],
                'index': index,
            }
            last = m.group(1)
            continue
        m = regex['mode'].search(line)
        if last and m:
            r = {
                'width': int(m.group(1)),
                'height': int(m.group(2)),
                'rate': float(m.group(3)),
            }
            query[last]['modes'].append(r)
            if (m.group(4) == '*'): query[last]['current'] = r
            if (m.group(5) == '+'): query[last]['preferred'] = r
            continue
        last = None

    return query

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

def multi_head_setup(query):
    config = get_config(query)
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
    output = check_output(['xrandr'], universal_newlines=True)
    multi_head_setup(parse(output))

if __name__ == '__main__':
    main()
