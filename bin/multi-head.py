#!/usr/bin/env python

# This code is borrowed from: https://github.com/substack/xrandr-parse

from subprocess import check_output
import subprocess
import re

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
                'width': m.group(1),
                'height': m.group(2),
                'rate': float(m.group(3)),
            }
            query[last]['modes'].append(r)
            if (m.group(4) == '*'): query[last]['native'] = r
            if (m.group(5) == '+'): query[last]['current'] = r
            continue
        last = None

    return query

def multi_head_setup(query):
    connects = query.keys()
    connects = [ key for key in query if query[key]['connected'] ]
    disconnects = [ key for key in query if not query[key]['connected'] ]
    primary = 'LVDS1'
    if primary in connects:
        connects.remove(primary)
    else:
        primary = connects.pop()
    cmd = ['xrandr', '--output', primary, '--auto']
    if (len(connects) > 0):
        cmd += ['--output', connects[0], '--right-of', primary, '--auto']
    for output in disconnects:
        cmd += ['--output', output, '--auto']
    subprocess.call(cmd)

def main():
    output = check_output(['xrandr'], universal_newlines=True)
    multi_head_setup(parse(output))

if __name__ == '__main__':
    main()
