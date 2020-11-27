#!/bin/bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config
polybar main >> /tmp/polybarmain.log 2>&1 & disown
polybar right >> /tmp/polybarmain.log 2>&1 & disown


echo "Polybar launched..."
