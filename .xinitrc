#!/bin/sh

# Executed by startx (run your window manager from here)

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi
if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi
if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

#xrandr | grep VGA1 && xrandr --output VGA1 --auto --right-of LVDS1 --mode 1920x1200

# office monitors
[ `xrandr | egrep 'HDMI(2|3) connected' | wc -l` == 2 ] && xrandr --output HDMI2 --primary --auto --right-of HDMI3 --auto
# home monitors (nvidia official driver)
#[ `xrandr | egrep 'DP-(2|4) connected' | wc -l` == 2 ] && xrandr --output DP-2 --primary --auto --left-of DP-4 --auto
# home monitors (nouveo driver)
if [ `xrandr | egrep 'DP-(1|2) connected' | wc -l` == 2 ] ; then
    ## xrandr --output DP-1 --primary --auto --right-of DP-2 --auto
    xrandr --output DP-2 --primary --auto --output DP-1 --off
fi

# if this is Thinkpad
#type dockd > /dev/null 2>&1 && dockd --daemon

[ -f ~/.xinitrc-local ] && source ~/.xinitrc-local

exec dbus-launch --sh-syntax --exit-with-session i3 --shmlog-size=26214400

# vim: ft=sh

