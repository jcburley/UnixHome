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
- Make sure `~/.profile`, if it exists, does *not* run `.bashrc` if it is being run by `etc/bash_profile` (linked from `~/.bash_profile`):
  - `if [ -n "$BASH_VERSION" ] && [ "$WILL_RUN_BASHRC" != "true" ]`
  - The `~/.profile` created by MacPorts doesn't try to run `.bashrc` or anything else, so should not be affected; but Ubuntu 16.04, or something I installed above it on `dove`, had one that did in `~craig/.profile`.
  - `__git_ps1` not being found at each prompt is a symptom of this mechanism breaking, since `etc/git-prompt.sh` needs to be sourced from `${UNIXHOME}`, which `~/.bash_profile` defines only after invoking `~/.profile` (which could perhaps be changed, but it's really not clear to me which startup script should be responsible for what actions across all OSes and shells, so the order in which things should be done is also unclear).

Then:
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

