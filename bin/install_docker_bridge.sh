#!/bin/bash

RESPONSE=""
CURDUR=""
TMPDIR=""
GOVERSION="1.11.5"
DOCKERVERSION="18.09.1"
DOCKERCOMPOSEVERSION="1.23.2"
wHomeWinPath=$(cmd.exe /c 'echo %HOMEDRIVE%%HOMEPATH%' 2>&1 | tr -d '\r')
wHome=$(wslpath -u "${wHomeWinPath}")

function createtmp {
    echo "Saving current directory as \$CURDIR"
    CURDIR=$(pwd)
    TMPDIR=$(mktemp -d)
    echo "Going to \$TMPDIR: $TMPDIR"
    cd $TMPDIR
}

function cleantmp {
    if [ -n "$CURDIR" ]; then
        echo "Returning to $CURDIR"
        cd $CURDIR
    fi
    if [ -n "$TMPDIR" ]; then
        echo "Cleaning up $TMPDIR"
        sudo rm -r $TMPDIR  # need to add sudo here because git clones leave behind write-protected files
    fi
}

function _info() {
    printf "\e[1;36m::\e[1;37m $1 \e[00m \n"
}

function _die() {
    printf "\e[1;37m::\e[1;31m $1 \e[00m \n"
    cleantmp
    exit 1
}

function installmenu {

    CHOICE=$(
    whiptail --title "wsl-setup" --checklist --separate-output "\nHand-curated add-ons [SPACE to select, ENTER to confirm]:" 22 120 10 \
        "GO" "Install/Update the latest Go from Google" off \
        "DOCKER_BRIDGE" "Install a secure bridge to Docker for Windows" off \
        "DOCKER_INSTALL" "Install/Update Docker and Docker Composer inside WSL" off \
        "EXIT" "EXIT" off \
        "NONE" "" on 3>&1 1>&2 2>&3
    )

    echo "Selected:" $CHOICE

    if [[ $CHOICE == *"DOCKER_BRIDGE"* ]] ; then
        echo "DOCKER_BRIDGE"
        docker_bridge_install
    fi

    if [[ $CHOICE == *"DOCKER_INSTALL"* ]] ; then
        echo "DOCKER_INSTALL"
        docker_install
    fi

    if [[ $CHOICE == *"GO"* ]] ; then
        echo "GO"
        goinstall
    fi

    if [[ $CHOICE == *"EXIT"* ]] ; then
        _info "Enjoy!"
        exit 0
    fi

    if [[ $CHOICE == "NONE" ]] ; then
        echo "NONE"
        whiptail --title "No selection" --msgbox "Please select your desired options using the SPACE bar and then hit ENTER to confirm." 8 80
        installmenu
    fi
}

function goinstall {
    GOVERSION=$(whiptail --inputbox "What GO version you like to install??" 8 78 "$GOVERSION" --title "Installing GO" 3>&1 1>&2 2>&3)
    exitstatus=$?

    if [ $exitstatus = 0 -o -n "$GOVERSION" ]; then
        createtmp
        _info "Downloading Go using wget."
        wget https://dl.google.com/go/go${GOVERSION}.linux-$(dpkg --print-architecture).tar.gz
        exitstatus=$?

        if [ ! $exitstatus -eq 0 ]; then
            _die "Failed to download GO, bye."
        fi
        if [ ! tar tzf go*.tar.gz > /dev/null ]; then
            _die "Failed to download GO, bye."
        fi

        _info "Unpacking tar binaries to /usr/local/go."
        sudo tar -C /usr/local -xzf go*.tar.gz

        _info "Creating ~/go/ for your projects."
        mkdir ~/go/

        _info "Setting Go environment variables GOROOT, GOPATH, and adding Go to PATH with export."
        export GOROOT=/usr/local/go
        export GOPATH=$HOME/go/
        export PATH=$GOPATH/bin:$GOROOT/bin:/usr/local/go/bin:$PATH

        _info "Saving Go environment variables to $HOME/.bashlocal so they will persist."
        sudo sh -c 'echo "export GOROOT=/usr/local/go" >> $HOME/.bashlocal'
        sudo sh -c 'echo "export GOPATH=\${HOME}/go/" >> $HOME/.bashlocal'
        sudo sh -c 'echo "export PATH=\${GOPATH}/bin:\${GOROOT}/bin:/usr/local/go/bin:\${PATH}" >> $HOME/.bashlocal'

        _info "Sourcing ~/.bashlocal"
        . $HOME/.bashlocal

        cleantmp
    else
        _die "Invalid GO version"
    fi
}

function dockerinstall_build_relay {

    #Build the relay
    if [[ ! -f "${wHome}/.npiperelay/npiperelay.exe" ]]; then
        _info "Compiling npiperelay"

        if ! type go > /dev/null 2>&1; then
            _info "Go is needed to build npiperelay for Docker brdige"
            goinstall
        fi

        mkdir gohome
        export GOPATH=$(pwd)/gohome

        if ! (git version); then
            sudo apt-get install -yq git
        fi

        _info "Building npiperelay.exe."
        go get -d github.com/jstarks/npiperelay

        GOOS=windows go build -o npiperelay.exe github.com/jstarks/npiperelay
        sudo mkdir -p "${wHome}/.npiperelay"
        cmd.exe /c 'attrib +h %HOMEDRIVE%%HOMEPATH%\.npiperelay'
        sudo cp npiperelay.exe "${wHome}/.npiperelay/npiperelay.exe"
    fi

    sudo apt-get -y -q install socat

    cat << 'EOF' >> docker-relay
#!/bin/bash
connected=$(docker version 2>&1 | grep -c "daemon\|error")
if [[ ${connected} != 0  ]]; then
    PATH=${PATH}:$(wslpath "C:\Windows\System32")
    wHomeWinPath=$(cmd.exe /c 'echo %HOMEDRIVE%%HOMEPATH%' 2>&1 | tr -d '\r')
    wHome=$(wslpath -u "${wHomeWinPath}")
    killall --quiet socat
    exec nohup socat UNIX-LISTEN:/var/run/docker.sock,fork,group=docker,umask=007 EXEC:"\'${wHome}/.npiperelay/npiperelay.exe\' -ep -s //./pipe/docker_engine",nofork  </dev/null &>/dev/null &
fi
EOF
    sudo cp docker-relay /usr/bin/docker-relay
    sudo chmod +x /usr/bin/docker-relay

    echo '%sudo   ALL=NOPASSWD: /usr/bin/docker-relay' | sudo EDITOR='tee -a' visudo --quiet --file=/etc/sudoers.d/docker-relay

    cat << 'EOF' >> docker_relay.sh
sudo docker-relay
EOF
    sudo cp docker_relay.sh /etc/profile.d/docker_relay.sh
    sudo chmod -w /usr/bin/docker-relay

    sudo addgroup docker
    sudo adduser ${USER} docker

    _info "Running the relay for the first time."
    sudo docker-relay

    sleep 1s

    sudo docker version
}

function dockerinstall_conf_tcp {

    _info "Connect to Docker via TCP"

    cat << 'EOF' >> docker_relay.sh
export DOCKER_HOST=tcp://0.0.0.0:2375
EOF
    sudo cp docker_relay.sh /etc/profile.d/docker_relay.sh

    export DOCKER_HOST=tcp://0.0.0.0:2375
    connected=$(docker version 2>&1 | grep -c "Cannot connect to the Docker daemon")
    if [[ ${connected} != 0  ]]; then
        whiptail --title "DOCKER" \
        --msgbox "Please go to Docker for Windows -> Settings -> General and enable 'Expose daemon on tcp://localhost:2375 without TLS' or upgrade your Windows version and run this script again." 9 75
    fi
}

function docker_install {
    sudo apt-get update -yq
    sudo apt-get install -yq bash-completion

    DOCKERVERSION=$(whiptail --inputbox "What Docker version you like to install??" 8 78 "$DOCKERVERSION" --title "Installing Docker" 3>&1 1>&2 2>&3)
    exitstatus=$?

    if [ $exitstatus -eq 0 -o -n "$DOCKERVERSION" ]; then
        _info "Downloading docker"
        curl -L -O https://download.docker.com/linux/static/stable/$(uname -m)/docker-${DOCKERVERSION}.tgz
        exitstatus=$?

        if [ ! $exitstatus -eq 0 -o ! `tar tzf docker-${DOCKERVERSION}.tgz > /dev/null` ]; then
            _die "Failed to download docker, bye."
        fi

        _info "Downloading docker bash-completion"
        curl -L -o docker-bash-completion https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/bash/docker # /etc/bash_completion.d/docker'
        exitstatus=$?

        if [ ! $exitstatus -eq 0 ]; then
            _die "Failed to download docker bash-completion, bye."
        fi
    else
        _die "Invalid Docker version, bye"
    fi

    DOCKERCOMPOSEVERSION=$(whiptail --inputbox "What Docker Compose version you like to install??" 8 78 "$DOCKERCOMPOSEVERSION" --title "Installing Docker Compose" 3>&1 1>&2 2>&3)
    exitstatus=$?

    if [ $exitstatus -eq 0 -o -n "$DOCKERCOMPOSEVERSION" ]; then
        _info "Downloading docker-compose"
        curl -L -o docker-compose https://github.com/docker/compose/releases/download/${DOCKERCOMPOSEVERSION}/docker-compose-`uname -s`-`uname -m`
        exitstatus=$?

        if [ ! $exitstatus -eq 0 ]; then
            _die "Failed to download docker-compose, bye."
        fi

        _info "Downloading docker-compose bash-completion"
        curl -L -o docker-compose-completion https://raw.githubusercontent.com/docker/compose/${DOCKERCOMPOSEVERSION}/contrib/completion/bash/docker-compose
        exitstatus=$?

        if [ ! $exitstatus -eq 0 ]; then
            _die "Failed to download docker-compose bash-completion, bye."
        fi
    else
        _die "Invalid docker-compose version, bye"
    fi

    _info "Extracting docker-${DOCKERVERSION}.tgz to /usr/bin"
    sudo tar -xzvf docker-${DOCKERVERSION}.tgz --overwrite --directory /usr/bin/ --strip-components 1 docker/docker
    [ ! `echo $?` -eq 0 ] && _die "Failed to extract docker-${DOCKERVERSION}.tgz to /usr/bin"

    sudo chmod 755 /usr/bin/docker
    [ ! `echo $?` -eq 0 ] && _die "failed to chmod"

    sudo chown root:root /usr/bin/docker
    [ ! `echo $?` -eq 0 ] && _die "failed to chown"

    _info "Installing docker bash-completion"
    sudo cp docker-bash-completion /etc/bash_completion.d/docker
    [ ! `echo $?` -eq 0 ] && _die "failed to install docker bash-completion"

    _info "Installing docker-compose"
    sudo cp docker-compose /usr/bin/docker-compose
    [ ! `echo $?` -eq 0 ] && _die "failed to install docker-compose"

    sudo chmod +x /usr/bin/docker-compose
    [ ! `echo $?` -eq 0 ] && _die "failed to chmod"

    _info "Installing docker-compose bash-completion"
    sudo cp docker-compose-completion /etc/bash_completion.d/docker-compose
    [ ! `echo $?` -eq 0 ] && _die "failed to install docker-compose bash-completion"
}

function docker_bridge_install {
if (whiptail --title "DOCKER" --yesno "Would you like to install the bridge to Docker?" 8 55); then
    _info "Installing the bridge to Docker."

    export PATH=${PATH}:$(wslpath "C:\Windows\System32") #Be sure we can execute Windows commands

    connected=$(docker.exe version 2>&1 | grep -c "docker daemon is not running.\|docker.exe: command not found")
    while [[ ${connected} != 0  ]]; do
        if ! (whiptail --title "DOCKER" --yesno "Docker Desktop appears not to be running, please check it and ensure that it is running correctly. Would you like to try again?" 9 75); then
            return
        fi

        connected=$(docker.exe version 2>&1 | grep -c "docker daemon is not running.\|docker.exe: command not found")
    done

    createtmp

    if [ ! `type docker > /dev/null 2>&1` -o ! `type docker-compose > /dev/null 2>&1` ]; then
        docker_install
    fi

    #Checks if the Windows 10 version supports Unix Sockets and that the tcp port without TLS is not already open
    connected=$(env DOCKER_HOST=tcp://0.0.0.0:2375 docker version 2>&1 | grep -c "Cannot connect to the Docker daemon")
    if [[ $(reg.exe query "HKLM\Software\Microsoft\Windows NT\CurrentVersion" /v "CurrentBuild" 2>&1 | egrep -o '([0-9]{5})' | cut -d ' ' -f 2) -gt 17063 && ${connected} != 0  ]]; then
        #Connect via Unix Sockets
        whiptail --title "DOCKER" --msgbox "Your Windows supports Unix sockects, configuring it" 8 60
        dockerinstall_build_relay
    else
        #Connect via TCP
        whiptail --title "DOCKER" --msgbox "Your Windows is not supporting Unix sockects, connecting by TCP" 8 60
        dockerinstall_conf_tcp
    fi

    if [[ $(wslpath 'C:\\') = '/mnt/c/' ]]; then

        if (whiptail --title "DOCKER" --yesno "To correctly integrate the volume mounting between docker Linux and Windows, your root mount point must be changed from /mnt/c to /c. Continue?" 10 80); then
            echo "Changing the root from /mnt to /"

            if [[ $(grep -c "root" /etc/wsl.conf) -eq 0 ]]; then
                sudo sed -i 's$\[automount\]$\0\nroot=/$' /etc/wsl.conf

            else
                sudo sed -i 's$\(root=\)\(.*\)$\1/$' /etc/wsl.conf
            fi

        fi
    fi

    whiptail --title "DOCKER" --msgbox "Docker bridge is ready. Please close and re-open WLinux" 8 60
    cleantmp
else
    _info "Skipping Docker Bridge"
fi
}

installmenu
