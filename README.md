# UnixHome
My $HOME directory for Unixy OSes

# Before Cloning Me

## Per-OS Preparation

### Mac OS X

#### Establish your login shell

These instructions tend to assume Bash, so (if necessary) `chsh -s /bin/bash` and login a new shell to use going forward.

#### Consider GitHub Desktop

Optionally, install GitHub Desktop via https://desktop.github.com/ and login.

#### Install Homebrew

Install Homebrew ("brew") from the latest release at https://github.com/Homebrew/brew/releases (for a `.pkg` file) or https://brew.sh (the official installation script).

**Crucial:** Do not manually symlink the `brew` binary to other locations (like `/usr/local/bin/brew` on Apple Silicon).
Homebrew uses path-based architecture detection; invoking it via a symlink tricks it into assuming the wrong prefix and platform environment.

Once the installation completes, you must add Homebrew to your shell environment so your terminal can locate it naturally without symlinks.

1. **Configure your profile:** Run the configuration tool built directly into Homebrew to append the native environment variables to your login profile:
   
   ```bash
   # For Bash users:
   echo 'eval "\$(/opt/homebrew/bin/brew shellenv)"' >> ~/.bash_profile

   # For Zsh users (macOS default):
   echo 'eval "\$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
   ```

2. **Apply the changes immediately:** Load the environment into your active terminal session without needing to restart it:
   
   ```bash
   # For Bash users:
   source ~/.bash_profile

   # For Zsh users:
   source ~/.zprofile
   ```

3. **Verify the installation:** Confirm that your shell correctly targets the native ARM64 prefix path:
   
   ```bash
   which brew
   # Expected output: /opt/homebrew/bin/brew
   ```

#### Install GNU stuff

```sh
brew install coreutils findutils gnu-tar gsed gnutls gnu-indent gnu-getopt gawk grep bash-completion zsh-completion emacs
```

**NOTE:** This seems to take awhile before the new Emacs is actually invoked (try `emacs --version`). Maybe try `hash -r` first, or log in a new terminal and try it there.

Per the output of `brew`, to have various commands such as `tar` (not only when prefixed with `g`, e.g. `gtar`) run GNU versions instead of native ones, edit `~/.profile` to specify e.g. `PATH="/usr/local/opt/gnu-tar/libexec/gnubin:$PATH"`.

#### Name Your Mac

If the system is under your control, make sure system name (as returned by `hostname`) is as desired for `.emacs.d/systems/`:
  - `sudo scutil --set HostName xxx`
  - `sudo scutil --set LocalHostName xxx`
  - `sudo scutil --set ComputerName xxx`
 
### Windows

- Install Git from e.g.: https://git-scm.com/downloads/win
  - This might be the best way to get a working Bash (Git Bash)
  - It also offers a Git UI that is not necessary for the present purpose
  - Configure it as "Unix-like" as you feel comfortable; the less Unix-like, the more likely aliases, functions, and scripts herein might misbehave; the more Unix-like, the more likely Windows scripts might.

## All OSes

Add local account's public ED25519 key (on Linux, this will be in `~/.ssh/id_ed25519.pub`, or do `ssh-keygen -t ed25519` to create it) as an SSH key to your https://github.com account, currently via https://github.com/settings/keys. (This might not be necessary, or might be differently handled, if one uses GitHub Desktop.)

Install, if not already present, GNU Emacs from a trusted source, such as: https://www.gnu.org/software/emacs/download.html

# Clone Me

Do either this:
```
cd ~; git clone git@github.com:jcburley/UnixHome.git .unixhome
```

Or, use GitHub Desktop
  - Change the desired target directory to specify `~/.unixhome` (e.g. remove intermediate components, lowercase capitalized "UnixHome", etc.)

# After Cloning Me

```
cd ~/.unixhome/Setup
./git
./bash
./emacs
cd ../bin
./install.sh  # Installs to ~/bin: bash-for-emacs emacs install-go.sh path rssh settitle
# sudo ./install.sh --system # Optional, instead of './install.sh', to install to /usr/local/bin
# cd ../build; ./install.sh  # OPTIONAL, if the 'build' command is desired
```

## GNU Emacs on Windows

Now that `~/.profile` has been created, on Windows it's probably necessary to append this line to it:

```
PATH="$PATH:/c/Program Files/Emacs/emacs-30.1/bin"
```

(The actual version number will depend on which version of GNU Emacs you installed, above.)

Then start up a new Git Bash window for the changes to take effect.

## Why Install to /usr/local/bin?
Installing useful tools into `/usr/local/bin`, instead of `~/bin`,
by using (say) the `--system` option with `./install.sh` in a directory,
simplifies access via e.g. `ssh user@system <toolname>`. E.g. on my
Ubuntu 16.04 system, I can't figure out how to add `~/bin` to
`$PATH` such that it is effective when searching for *toolname* in
such an **ssh** command. (Seems like `/bin/sh`, which is **dash**, gets
invoked by **sshd** only to initialize things, not to establish an
environment that's then passed to the *toolname* when it is actually
executed. No amount of playing around with `/etc/environment`, adding
`PermitUserEnvironment yes` to `/etc/ssh/sshd_config`, putting
definitions in `~/.ssh/rc`, etc., seemed to have any effect on the
environment for *toolname* despite it having various effects on the
output if *toolname* was something like `env`.)

# Sanity-check
## Shell Startup (Profiles and Aliases and Functions, Oh My!)
Make sure that `etc/bashrc` and `etc/bash_profile` get run by various methods of logging in, but do not get caught in a loop running each other (or being run by outside scripts).
- `~/.profile` typically doesn't try to run `.bashrc` or anything else, but Ubuntu 16.04, or something I installed above it on an Ubuntu machine, had one that did.
- `__git_ps1` not being found at each prompt is a symptom of this mechanism breaking, since `etc/git-prompt.sh` needs to be sourced from `${UNIXHOME}`, which `~/.bash_profile` defines only after invoking `~/.profile` (which could perhaps be changed, but it's really not clear to me which startup script should be responsible for what actions across all OSes and shells, so the order in which things should be done is also unclear).

## GNU Emacs
Start up GNU Emacs, and confirm:
- Personal bindings (such as `C-c w` to compare windows) work
- Finding a file (even if non-existent) such as `foo.joke` brings up Clojure and related modes (might have to `package-install` them)

# To Keep Installation Up To Date

## Per-OS Maintenance
### Mac OS X
```
brew upgrade -y
```

### Windows
If you chose the option to check daily for Git upgrades, that should handle Git Bash.

For Emacs and other utilities, check the download links periodically and take appropriate action.
