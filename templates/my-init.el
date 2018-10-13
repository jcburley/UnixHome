;;; package --- my .emacs config

;; No easy symlink support (i.e. via 'ln' in Git Bash) in Windows, so
;; just load the file directly everywhere.

(message (concat load-file-name " loading!"))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(load-file "~/.unixhome/components/emacs/init.el")
