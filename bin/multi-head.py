#!/usr/bin/env python

from __future__ import unicode_literals

from subprocess import check_output, Popen, PIPE, CalledProcessError
from functools import cmp_to_key
import json
import io

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

def main():
    output = check_output(['xrandr2json'], universal_newlines=True)
    query = json.loads(output)
    config = get_config(query)
    json_input = json.dumps(config)
    cmd = ['json2xrandr']
    process = Popen(cmd, universal_newlines=True, stdin=PIPE)
    output, err = process.communicate(json_input)
    retcode = process.poll()
    if retcode:
        raise CalledProcessError(retcode, cmd, output=output)

if __name__ == '__main__':
    main()
