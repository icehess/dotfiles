compton -b --backend glx --inactive-dim -m
xmodmap ~/.Xmodmap
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

#xrandr | grep VGA1 && xrandr --output VGA1 --auto --right-of LVDS1 --mode 1920x1200
if [ -n "`xrandr | grep 'DP-2 connected'`" ] ; then
    if [ -n "`xrandr | grep 'DP-4 connected'`" ] ; then
        echo hello
        xrandr --output DP-2 --auto --left-of DP-4 --auto
    fi
fi


kwalletd5 &
#dbus-update-activation-environment --systemd DISPLAY
#eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
#export SSH_AUTH_SOCK
~/.config/autostart-scripts/ssh-add.sh

pasystray &
dropbox &

/usr/lib/mate-polkit/polkit-mate-authentication-agent-1 &

exec i3

# vim: ft=sh
