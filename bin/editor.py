#!/usr/bin/env python

from __future__ import unicode_literals

import os
import sys
import stat
import socket
import re

def find_socket_name():
    socket_name = None
    tmpdir = os.environ.get('TMPDIR') or '/tmp'
    socket_dir = os.path.join(tmpdir, 'emacs{0}'.format(os.geteuid()))
    if not os.path.isdir(socket_dir):
        return None
    for filename in os.listdir(socket_dir):
        filepath = os.path.join(socket_dir, filename)
        if not stat.S_ISSOCK(os.stat(filepath).st_mode):
            continue
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        try:
            sock.connect(filepath)
            socket_name = filepath
            break
        except:
            continue
        finally:
            sock.close()
    return socket_name

def find_server_file():
    server_file = None
    home = os.environ['HOME']
    if not home:
        return None
    server_dir = os.path.join(home, '.emacs.d', 'server')
    if not os.path.isdir(server_dir):
        return None
    pattern = re.compile(b'^([^ ]+) ([^\n]+)\n(.+)$')
    for filename in os.listdir(server_dir):
        filepath = os.path.join(server_dir, filename)
        with open(filepath, 'rb') as f:
            cnt = f.read()
            match = pattern.match(cnt)
            address = match.group(1)
            colon_pos = address.rindex(b':')
            host = address[:colon_pos]
            port = int(address[colon_pos + 1:])
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            try:
                sock.connect((host, port))
                server_file = filepath
                break
            except:
                continue
            finally:
                sock.close()
    return server_file

def add_arguments(argv):
    # Convert argv from str list to unicode list in PY2
    if sys.version_info[0] == 2:
        argv = [ arg.decode(sys.getfilesystemencoding()) for arg in argv ]

    if 'EMACS_SERVER_FILE' in os.environ:
        return argv

    for opt in '-s', '--socket-name', '-f', '--server-file':
        if opt in argv:
            return argv

    socket_name = find_socket_name()
    if socket_name:
        return ['--socket-name', socket_name] + argv

    server_file = find_server_file()
    if server_file:
        return ['--server-file', server_file] + argv

def main():
    argv = add_arguments(sys.argv[1:])
    prog = 'emacsclient'
    argv = [prog] + argv
    os.execvp(prog, argv)

if __name__ == '__main__':
    main()
