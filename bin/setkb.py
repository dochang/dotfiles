#!/usr/bin/env python

from subprocess import check_output, call
import re

default_options=[
    'ctrl:nocaps',
    'altwin:swap_alt_win',
]

keyboards = [
    'AT Translated Set 2 keyboard',
    'HID 04d9:1203',
    ['HID 0188:6796', 'altwin:swap_alt_win'],
]

def main():
    xinput_list = check_output(['xinput', 'list'], universal_newlines=True)
    for item in keyboards:
        if isinstance(item, str):
            kb = item
            options = default_options
        else:
            kb = item[0]
            options = item[1:]
        match = re.search(kb + '.*id=([0-9]*).*keyboard', xinput_list)
        if match:
            device_id = match.group(1)
            cmd = ['setxkbmap', '-device', device_id]
            for opt in options:
                cmd.append('-option')
                cmd.append(opt)
            call(cmd)

if __name__ == '__main__':
    main()
