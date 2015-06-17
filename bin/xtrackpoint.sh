#!/bin/sh

### TrackPoint
## http://www.thinkwiki.org/wiki/How_to_configure_the_TrackPoint
## http://okomestudio.net/biboroku/?p=1816
## https://wiki.debian.org/InstallingDebianOn/Thinkpad/Trackpoint
## evdev(4)
if xinput list "TPPS/2 IBM TrackPoint" >/dev/null ; then
	# Enable vertical scrolling.
	xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
	xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
	# Enable horizontal scrolling in addition to vertical scrolling.
	#
	# 6 : X up ; 7 : X down ; 4 : Y up ; 5 : Y down
	xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
	# To enable middle button emulation (using left- and right-click simultaneously)
	xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Middle Button Emulation" 1
	# Switch touchpad off if trackpoint exists.
	synclient TouchpadOff=1
fi
