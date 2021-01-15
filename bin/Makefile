first: all

all:

TOOLS=\
bash-for-emacs \
path \
rssh \
settitle \

install:
# Installing useful tools into /usr/local/bin, instead of ~/bin,
# simplifies access via e.g. 'ssh user@system <toolname>'. E.g. on my
# Ubuntu 16.04 system ("doe"), I can't figure out how to add ~/bin to
# $PATH such that it is effective when searching for <toolname> in
# such an ssh command. (Seems like /bin/sh, which is dash, gets
# invoked by sshd only to initialize things, not to establish an
# environment that's then passed to the <toolname> when it is actually
# executed. No amount of playing around with /etc/environment, adding
# "PermitUserEnvironment yes" to /etc/ssh/sshd_config, putting
# definitions in ~/.ssh/rc, etc., seemed to have any effect on the
# environment for <toolname>, despite it having various effects on the
# output if <toolname> was something like 'env'.)
	install -p $(TOOLS) /usr/local/bin
	(cd /usr/local/bin; chmod u-w $(TOOLS))

.PHONY: first all install
