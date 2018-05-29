# UnixHome
My $HOME directory for Unixy OSes

# Before Cloning Me

## Mac OS X:
- Install MacPorts via: www.macports.org/install.php  # this will create ~/.profile
- Install GitHub Desktop and login
- Install homebrew ("brew")
- Install GNU stuff via:
    `sudo port install coreutils findutils gnutar gsed gnutls gindent getopt gawk grep emacs`
- NOTE: Seems to take awhile, or maybe a `$ hash -r`, for the Emacs install to take effect; log in a new terminal
- Edit ~/.profile to specify `/opt/local/libexec/gnubin` before `$PATH`, per MacPorts docs

$ ./.Setup/git  # might be needed before cloning me (UnixHome).
$ mkdir -p ~/github
$ cd ~/github

# Clone Me

$ git clone  # clone me.

# After Cloning Me
```
$ cd ~/github/UnixHome/.Setup
$ ./bash

```

# To keep installation up to date:

## Mac OS X:
```
$ port selfupdate
```

