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
- Edit `~/.profile` to specify `/opt/local/libexec/gnubin` before `$PATH`, per MacPorts docs
- Make sure system name (as returned by `hostname`) is as desired for `.emacs.d/systems/`:
  - `sudo scutil --set HostName xxx`
  - `sudo scutil --set LocalHostName xxx`
  - `sudo scutil --set ComputerName xxx`

## All OSes
Double-check that `etc/bashrc` and `etc/bash_profile` get run by various methods of logging in, but do not get caught in a loop running each other (or being run by outside scripts).
- The `~/.profile` created by MacPorts doesn't try to run `.bashrc` or anything else, but Ubuntu 16.04, or something I installed above it on `dove`, had one that did in `~craig/.profile`.
- `__git_ps1` not being found at each prompt is a symptom of this mechanism breaking, since `etc/git-prompt.sh` needs to be sourced from `${UNIXHOME}`, which `~/.bash_profile` defines only after invoking `~/.profile` (which could perhaps be changed, but it's really not clear to me which startup script should be responsible for what actions across all OSes and shells, so the order in which things should be done is also unclear).

Add local account's public RSA key (on Linux, this will be in `~/.ssh/id_rsa.pub`, or do `ssh-keygen -t rsa` to create it) as an SSH key to https://github.com account. (This might not be necessary if one uses GitHub Desktop.)

Then:
```
$ mkdir -p ~/github
$ cd ~/github
```

# Clone Me

Either of these:
- `$ git clone git@github.com:jcburley/UnixHome.git`
- Use GitHub Desktop
  - Change the desired target directory to specify `~/github/UnixHome` (e.g. remove intermediate components, lowercase capitalized "GitHub", etc.)

# After Cloning Me
```
$ cd ~/github/UnixHome/.Setup
$ ./git
$ ./bash
$ ./emacs
```

# To Keep Installation Up To Date

## Per-OS Maintenance
### Mac OS X
```
$ port selfupdate
```

