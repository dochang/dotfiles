#!/bin/sh

### TrackPoint
## http://www.thinkwiki.org/wiki/How_to_configure_the_TrackPoint
## http://okomestudio.net/biboroku/?p=1816
## https://wiki.debian.org/InstallingDebianOn/Thinkpad/Trackpoint
## xinput(1)
## evdev(4)
## libinput(4)

TRACKPOINT_DEV="TPPS/2 IBM TrackPoint"
TOUCHPAD_DEV="SynPS/2 Synaptics TouchPad"
SANWA_TRACKBALL="HID 04d9:1166"

configure_evdev_trackpoint() {
	# Enable vertical scrolling.
	xinput --set-prop "${TRACKPOINT_DEV}" "Evdev Wheel Emulation" 1
	xinput --set-prop "${TRACKPOINT_DEV}" "Evdev Wheel Emulation Button" 2
	# Enable horizontal scrolling in addition to vertical scrolling.
	#
	# 6 : X up ; 7 : X down ; 4 : Y up ; 5 : Y down
	xinput --set-prop "${TRACKPOINT_DEV}" "Evdev Wheel Emulation Axes" 6 7 4 5
	# To enable middle button emulation (using left- and right-click simultaneously)
	xinput --set-prop "${TRACKPOINT_DEV}" "Evdev Middle Button Emulation" 1
}

if xinput --list --name-only "${TRACKPOINT_DEV}" >/dev/null ; then
	if xinput --list-props "${TRACKPOINT_DEV}" | grep Evdev >/dev/null ; then
		configure_evdev_trackpoint
	fi
	# Disable touchpad if trackpoint exists.
	xinput --disable "${TOUCHPAD_DEV}"
fi

if xinput --list --name-only "${SANWA_TRACKBALL}" >/dev/null ; then
	# Enable middle button scroll on SANWA trackball
	xinput --set-prop "${SANWA_TRACKBALL}" "libinput Scroll Method Enabled" 0 0 1
	# Make it slow.  The default speed is too fast...
	xinput --set-prop "${SANWA_TRACKBALL}" "libinput Accel Speed" -0.5
fi
