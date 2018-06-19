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
if [ -n "`xrandr | grep 'HDMI2 connected'`" ] ; then
    if [ -n "`xrandr | grep 'HDMI3 connected'`" ] ; then
        xrandr --output HDMI2 --primary --auto --right-of HDMI3 --auto
    fi
fi

nitrogen --restore; sleep 1
compton -b

/usr/lib/pam_kwallet_init
kwalletd5 &
~/.config/autostart-scripts/ssh-add.sh
/usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &

gnome-screensaver &
xautolock -time 30 -locker "gnome-screensaver-command --lock" &

greenclip daemon &
pa-applet &
#pasystray &

dropbox &

exec dbus-launch --sh-syntax --exit-with-session i3 --shmlog-size 0

# vim: ft=sh
