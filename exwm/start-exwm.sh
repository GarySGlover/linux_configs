#!/bin/sh

# NOTE: This is only for the live demo, not needed for your configuration!
# spice-vdagent

# Screen locking suspend
xss-lock -- slock &

# Fire it up
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/.emacs.d/desktop.el
