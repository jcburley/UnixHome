# UnixHome
My $HOME directory for Unixy OSes

# Before Cloning Me

## Per-OS Preparation
### Mac OS X
- Install MacPorts via: www.macports.org/install.php
  - This will create `~/.profile`
- Install GitHub Desktop and login
- Install homebrew ("brew")
- Install GNU stuff via:
    `sudo port install coreutils findutils gnutar gsed gnutls gindent getopt gawk grep emacs`
  - NOTE: This seems to take awhile before the new Emacs is actually invoked (try `emacs --version`)
  - Maybe try `$ hash -r` first, or log in a new terminal and try it there
- Edit ~/.profile to specify `/opt/local/libexec/gnubin` before `$PATH`, per MacPorts docs

## All OSes
```
$ ./.Setup/git  # might be needed before cloning me (UnixHome).
$ mkdir -p ~/github
$ cd ~/github
```

# Clone Me

Either of these:
- `$ git clone git@github.com:jcburley/UnixHome.git # clone me.`
- Use GitHub Desktop
  - Change the desired target directory to specify `~/github/UnixHome` (e.g. remove intermediate components, lowercase capitalized "GitHub", etc.)

# After Cloning Me
```
$ cd ~/github/UnixHome/.Setup
$ ./bash
$ ./emacs
```

# To Keep Installation Up To Date

## Per-OS Maintenance
### Mac OS X
```
$ port selfupdate
```

