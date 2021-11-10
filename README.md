# UnixHome
My $HOME directory for Unixy OSes

# Before Cloning Me

## Per-OS Preparation
### Mac OS X
- Establish your login shell; these instructions assume Bash, so (if necessary) `chsh -s /bin/bash` and login a new shell to use going forward.
- Install MacPorts via: www.macports.org/install.php
  - This will create `~/.profile`, `~/.zprofile`, etc.
  - If the current or a new shell don't find the `port` command, this might work (if `~/.profile` invokes `/usr/libexec/path_helper -s`):
```shell
$ sudo sh -c 'echo /opt/local/bin > /etc/paths.d/opt-bin'
$ sudo sh -c 'echo /opt/local/sbin > /etc/paths.d/opt-sbin'
```
- Install GitHub Desktop and login
- Install homebrew ("brew")
- Install GNU stuff via:
    `sudo port install coreutils findutils gnutar gsed gnutls gindent getopt gawk grep emacs` (remove `emacs` if you're installing e.g. `emacs-app` via MacPorts)
  - NOTE: This seems to take awhile before the new Emacs is actually invoked (try `emacs --version`)
  - Maybe try `$ hash -r` first, or log in a new terminal and try it there
- Edit `~/.profile` to specify `/opt/local/libexec/gnubin` before `$PATH`, per MacPorts docs (see `$UNIXHOME/etc/sessions/craig@pony` for an example), or add that path to `/etc/paths.d/gnubin` as shown above
- If the system is under your control, make sure system name (as returned by `hostname`) is as desired for `.emacs.d/systems/`:
  - `sudo scutil --set HostName xxx`
  - `sudo scutil --set LocalHostName xxx`
  - `sudo scutil --set ComputerName xxx`

## All OSes
Add local account's public RSA key (on Linux, this will be in `~/.ssh/id_rsa.pub`, or do `ssh-keygen -t rsa` to create it) as an SSH key to https://github.com account. (This might not be necessary if one uses GitHub Desktop.)

# Clone Me
Do either of these:
- `$ cd ~; git clone git@github.com:jcburley/UnixHome.git .unixhome`
- Use GitHub Desktop
  - Change the desired target directory to specify `~/.unixhome` (e.g. remove intermediate components, lowercase capitalized "UnixHome", etc.)

# After Cloning Me
```
$ cd ~/.unixhome/Setup
$ ./git
$ ./bash
$ ./emacs
$ cd ../bin
$ sudo make install
$ cd ../build; sudo make install  # OPTIONAL, if the build command is desired
```

# Sanity-check
Make sure that `etc/bashrc` and `etc/bash_profile` get run by various methods of logging in, but do not get caught in a loop running each other (or being run by outside scripts).
- The `~/.profile` created by MacPorts doesn't try to run `.bashrc` or anything else, but Ubuntu 16.04, or something I installed above it on `dove`, had one that did in `~craig/.profile`.
- `__git_ps1` not being found at each prompt is a symptom of this mechanism breaking, since `etc/git-prompt.sh` needs to be sourced from `${UNIXHOME}`, which `~/.bash_profile` defines only after invoking `~/.profile` (which could perhaps be changed, but it's really not clear to me which startup script should be responsible for what actions across all OSes and shells, so the order in which things should be done is also unclear).

Start up GNU Emacs, and confirm:
- Personal bindings (such as `C-c w` to compare windows) work
- Finding a file (even if non-existent) such as `foo.joke` brings up Clojure and related modes (might have to `package-install` them)

# To Keep Installation Up To Date

## Per-OS Maintenance
### Mac OS X
```
$ sudo port selfupdate
```

