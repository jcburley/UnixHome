#!/bin/bash

if [ -z "$SUDO" ]; then
    SUDO=sudo
fi

function gethref ()
{
    HREF="$(grep href= "$1")"
    if [ $? -ne 0 ]; then
        echo >&2 "No href= found in $1"
        exit 14
    fi
    head -1 <<< "$HREF" | sed -e 's@.*href="\([^"]*\)".*@\1@'
}

if [ $(uname) != Darwin ]; then
    GLN=ln
    GREADLINK=readlink
else
    GLN=gln
    if ! which $GLN > /dev/null 2>/dev/null; then
        echo >&2 "Need 'gln' installed, so please install GNU coreutils first and then re-run."
        exit 18
    fi
    GREADLINK=greadlink
    if ! which $GREADLINK > /dev/null 2>/dev/null; then
        echo >&2 "Need 'greadlink' installed, so please install GNU coreutils first and then re-run."
        exit 18
    fi
fi

DOWNLOAD=
ECHO_OR_SUDO="$SUDO"
while true; do
    case "$1" in
        "-h" | "--help")
            echo "Usage: $0 [ -n | -d | -l ] [ <url-of-go-release> | <locally-available-version> ]"
            exit 99
            ;;
        "-d" | "--download")
            DOWNLOAD=yes
            shift
            ;;
        "-n" | "--dry-run")
            ECHO_OR_SUDO=echo
            shift
            ;;
        "-l" | "--list")
            ls -ld /usr/local/go*
            [ -d /usr/local/Cellar/go ] && ls -ld /usr/local/Cellar/go/*
            DONE=yes
            shift
            ;;
        -*)
            echo >&2 "Unrecognized option: $1"
            exit 98
            ;;
        *)
            break
    esac
done

if ! which go > /dev/null 2>/dev/null; then
    $ECHO_OR_SUDO $GLN -sfTv /usr/local/go/bin/go usr/local/bin/go
fi


if [[ "$1" =~ [.]tar[.]gz$ ]]; then
    mkdir -p ~/Downloads && cd ~/Downloads || exit $?

    PKG="$(basename $1)"
    VER="${PKG%.*-*.tar.gz}"
    WHERE="/usr/local/$VER"
    echo "PKG=$PKG VER=$VER WHERE=$WHERE"

    [ -f "$PKG" ] || curl -O "$1" && rm -fr go

    if [ ! -f "$PKG" ]; then
        echo >&2 "Cannot download $PKG"
        exit 12
    fi

    FTYPE="$(file --brief "$PKG")"
    case "$FTYPE" in
        *gzip*) ;;
        *HTML*)
            NEWURL=$(gethref "$PKG")
            rm -f "$PKG"
            curl -O "$NEWURL" || exit 15
            FTYPE="$(file --brief "$PKG")"
            if [[ ! "$FTYPE" =~ gzip ]]; then
                echo >&2 "URL's href yields file type \"$FTYPE\": $NEWURL"
                exit 16
            fi
            ;;
        *)
            echo >&2 "Unknown file type \"$FTYPE\" for $NEWURL"
            exit 13
    esac

    [ -d "$WHERE" -o -d go ] || tar xf "$PKG"

    [ -d "$WHERE" ] || $SUDO mv go "$WHERE"
    [ -z "$DOWNLOAD" ] && $SUDO $GLN -sfTv "$WHERE" /usr/local/go
    exit 0
fi

if [[ "$1" =~ [.]pkg$ ]]; then
    # Or: sudo installer -pkg /path/to/package.pkg -target /
    echo >&2 "Package (.pkg) not supported; use gzip'ed tar archive: $1"
    exit 19
fi

# Finish install on Darwin. Here, the user must have already done
# 'brew install' or 'brew upgrade' to do the install itself. This
# script then cleans up afterwards, by:
#   *  figuring out the version that was just installed ($version)
#   *  if necessary, moving the installed /usr/local/go directory to
#      /usr/local/go$version
#   *  symlinking /usr/local/go to the ultimate installed $GOROOT
#   *  ensuring /usr/bin/{go,gofmt} point to /usr/local/go/bin/

if [ $# -eq 0 ]; then
    [ -n "$DONE" ] && exit 0

    # Determine the just-installed version's full version number. This
    # might not be the same as what "go version" reports; e.g. "1.14.2_1"
    # was reporting "1.14.2" (and the latter was in $GOROOT/VERSION). To
    # protect against that, first try using the existing symlink (if any)
    # for the 'go' command, which should be of the form
    # /usr/local/Cellar/go/$version/bin/go. If there's no such symlink,
    # fall back to reading the version number printed by 'go version'.
    link="$($GREADLINK -f $(which go))"
    if [[ $? -eq 0 && "$link" =~ ^/usr/local/Cellar/go/.*/bin/go$ ]]; then
        version="$(basename $(dirname $(dirname $link)))"
        if [ "$version" = "libexec" ]; then
            version="$(basename $(dirname $(dirname $(dirname $link))))"
        fi
    else
        version="$(go version | (read IGN1 IGN2 VER IGN3; echo "$VER"))"
        if [ $? -ne 0 -o -z "$version" ]; then
            echo >&2 "Cannot determine version of 'go' command currently installed"
            exit 8
        fi
        version=${version/go/}
    fi

    # Determine ultimate (newly installed) $GOROOT. If
    # /usr/local/Cellar/go/$version, can just use that. If /usr/local/go,
    # rename it to /usr/local/go$version and use that.
    newroot="$(go env GOROOT)"
    if [ $? -ne 0 ]; then
        echo >&2 "'go' command currently does not work; consider 'GOROOT=<root-dir> GO.sh'"
        exit 11
    fi
    if [ ! -d "$newroot" ]; then
        echo >&2 "Newly installed root $newroot does not exist or is not a directory"
        exit 5
    fi
else
    if [[ "$1" =~ ":" ]]; then
        echo >&2 "On Mac OS X, use 'brew upgrade go' first, then invoke this script with no args."
        echo >&2 "Or, to install a different version already available, specify that version"
        echo >&2 "as the only argument on the command line."
        exit 99
    fi
    version="$1"
    newroot1="/usr/local/Cellar/go/$version/libexec"
    newroot2="/usr/local/go$version"
    if [ -d "$newroot1" ]; then
        newroot="$newroot1"
    elif [ -d "$newroot2" ]; then
        newroot="$newroot2"
    else
        echo >&2 "Cannot find $newroot1 nor $newroot2"
        exit 98
    fi
fi

ultimate_newroot="$($GREADLINK -f $newroot)"
if [ ! -d "$ultimate_newroot" ]; then
    echo >&2 "Installed root $newroot resolves, as a symlink, to something that does not exist or is not a directory"
    exit 10
fi
newroot="$ultimate_newroot"

# Make a reasonable adjustment if it'll work.
if [ ! -d "$newroot/api" -a -d "$newroot/libexec" -a -d "$newroot/libexec/api" ]; then
    newroot="$newroot/libexec"
fi

# Confirm a useful ultimate $GOROOT.
if [ ! -d "$newroot/api" ]; then
    echo >&2 "Not really a GOROOT-like directory at $newroot -- no api subdirectory"
    exit 9
fi

# If installed in /usr/local/go, move that to a version-named
# directory.
if [ "$newroot" = "/usr/local/go" ]; then
    verroot="/usr/local/go$version"

    # Move newly installed /usr/local/go into version-named directory.
    if [ -d "$verroot" ]; then
        echo >&2 "$verroot already exists, so not overwriting it with $newroot"
    else
        $ECHO_OR_SUDO mv -v $newroot "$verroot"
    fi
    newroot="$verroot"
else
    # Don't change an existing /usr/local/go directory.
    if [ -d "/usr/local/go" -a ! -L "/usr/local/go" ]; then
        echo >&2 "/usr/local/go is a directory; not putting a link to $verroot in it"
        exit 6
    fi
fi

# Symlink /usr/local/go to ultimate $NEWROOT.
$ECHO_OR_SUDO $GLN -svfT "$newroot" /usr/local/go

# Ensure /usr/local/bin/{go,gofmt} symlink to $NEWROOT/bin/.
$ECHO_OR_SUDO $GLN -svft /usr/local/bin/ /usr/local/go/bin/{go,gofmt}
