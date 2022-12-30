# Convert the parameters or STDIN to lowercase.
lc () {
    if [ $# -eq 0 ]; then
        tr '[:upper:]' '[:lower:]';
    else
        tr '[:upper:]' '[:lower:]' <<< "$@";
    fi;
}

# Convert the parameters or STDIN to uppercase.
uc () {
    if [ $# -eq 0 ]; then
        tr '[:lower:]' '[:upper:]';
    else
        tr '[:lower:]' '[:upper:]' <<< "$@";
    fi;
}

# Print the given text in the center of the screen.
function center {
    width=$(tput cols);
    str="$@";
    len=${#str};
    [ $len -ge $width ] && echo "$str" && return;
    for ((i = 0; i < $(((($width - $len)) / 2)); i++)); do
        echo -n " ";
    done;
    echo "$str";
}

function line {
    width=$(tput cols);
    str=${1--};
    len=${#str};
    for ((i = 0; i < $width; i += $len)); do
        echo -n "${str:0:$(($width - $i))}";
    done;
    echo;
}

_info () {
    printf "\e[1;36m::\e[1;37m $* \e[00m \n" >&2;
}

_error () {
    printf "\e[1;37m::\e[1;31m $* \e[00m \n" >&2;
}

_die () {
    _error "$1"
    exit 1
}

# use it like `loop_do "$SOME_DIRS" 'do your thing;'`.
# don't be evil though, eval is dangerous.
# I like `fn` from https://github.com/spencertipping/bash-lambda
#
# Also there are some other variation:
# (1) if no lazy evaluation, no need for eval
# function x()      { echo "Hello world";          }
# function around() { echo before; $1; echo after; }
# around x
#
# (2) same as above with args for x
# function x()      { echo "x(): Passed $1 and $2";  }
# function around() { echo before; "$@"; echo after; }
# around x 1st 2nd
#
# (3) using sub-shell; "x" must be a command or function
# function x() { pushd $1 ; git branch -vv | grep ": gone]" |  grep -v "\*" | awk '{ print $1; }' | xargs git branch -D ; popd; }
# for dir in $COMMIO ; do ("x" $dir) ; done
#
#
# Usage:
# loop_do "applications/*/" "do_in_dir \$loop_i 'git status --branch -s'"
# loop_do "applications/*/" "do_in_dir \$loop_i 'rm-if-gone'"
# loop_do "$COMMIO_APPS" "do_in_dir \$loop_i 'git commit -a -m \"setting base_branch to 1.0\"'"
# loop_do "$COMMIO_APPS" "do_in_dir \$loop_i 'nvim .circleci/config.yml'"
function loop_do() {
    local loop_i= stop= ret=
    case $1 in
        -stop)
            stop=1
            shift
            ;;
        *)
            ;;
    esac
    for loop_i in $1 ; do
        _info "doing $loop_i"
        eval $2
        ret=$?
        [ -n "$stop" ] && [ $ret -ne 0 ] && _error "command failed" && return
    done

    return $ret
}

function do_in_dir() {
    local ret=
    if [ ! -d "$1" ]; then
        echo "$1 does not exists"
        return 1
    fi
    pushd "$1" > /dev/null
    eval "$2"
    ret=$?
    popd > /dev/null
    return $ret
}
