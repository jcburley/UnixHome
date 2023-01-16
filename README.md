# UnixHome
My $HOME directory for Unixy OSes

# Before Cloning Me

## Per-OS Preparation
### Mac OS X
- Establish your login shell; these instructions assume Bash, so (if necessary) `chsh -s /bin/bash` and login a new shell to use going forward.
- Install GitHub Desktop via https://desktop.github.com/ and login
- Install homebrew ("brew")
- Install GNU stuff via:
    `brew install coreutils findutils gnu-tar gsed gnutls gindent getopt gawk grep bash-completion zsh-completion emacs`
  - NOTE: This seems to take awhile before the new Emacs is actually invoked (try `emacs --version`)
  - Maybe try `$ hash -r` first, or log in a new terminal and try it there
- Per the output of `brew`, to have the `tar` command (not only `gtar`) run GNU tar (instead of BSD tar), edit `~/.profile` to specify `PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"`
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
- `~/.profile` typically doesn't try to run `.bashrc` or anything else, but Ubuntu 16.04, or something I installed above it on an Ubuntu machine, had one that did.
- `__git_ps1` not being found at each prompt is a symptom of this mechanism breaking, since `etc/git-prompt.sh` needs to be sourced from `${UNIXHOME}`, which `~/.bash_profile` defines only after invoking `~/.profile` (which could perhaps be changed, but it's really not clear to me which startup script should be responsible for what actions across all OSes and shells, so the order in which things should be done is also unclear).

Start up GNU Emacs, and confirm:
- Personal bindings (such as `C-c w` to compare windows) work
- Finding a file (even if non-existent) such as `foo.joke` brings up Clojure and related modes (might have to `package-install` them)

# To Keep Installation Up To Date

## Per-OS Maintenance
### Mac OS X
```
brew upgrade
```

