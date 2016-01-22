#!/bin/sh

xrandr2json | multi-head.py | json2xrandr
