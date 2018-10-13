;;; package --- my .emacs config
;;; Commentary:
;;  2017-10-19 jcburley copied some stuff from github.com/vidjuheffex/dotemacs/
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(defvar username-at-systemname
  (concat (user-login-name) "@" (downcase (system-name)))
  "User login name and system name, separated by an atsign. E.g.: craig@doe")

(defvar session-specific-scratch-dir
 (concat "~/.emacs.d/sessions/" username-at-systemname)
 "session-specific scratch, cache, and other temporary files are stored here.")
(make-directory session-specific-scratch-dir t)

(defvar session-specific-init-dir
 (concat (getenv "UNIXHOME") "/.emacs.d/sessions/" username-at-systemname)
 "Session-specific customization and other files are stored here.")
(make-directory session-specific-init-dir t)

;; Put customizations in per-hostname files.
(setq custom-file (concat session-specific-init-dir "/customizations.el"))

;; Confirm whether it's okay to exit without saving customizations.
;; (If running a version of Emacs that does not support this, you
;; might have to use ESC-x kill-emacs to exit.)
(unless (< emacs-major-version 25)
  (add-hook 'kill-emacs-query-functions
	    'custom-prompt-customize-unsaved-options))

;; Even if no file-backed buffers have unsaved modifications, no
;; running shells exist, etc., prompt anyway, in case a 'git commit'
;; (or similar) is in progress, as these aren't necessarily
;; file-backed. (On Macs, I've accidentally done Cmd-Q instead of
;; Alt-Q, in the middle of writing such commit messages, several
;; times.)
(setq confirm-kill-emacs (quote yes-or-no-p))

(setq user-full-name "James Craig Burley")
(setq user-mail-address "james@burleyarch.com")
(setq global-mark-ring-max 200)

; Stuff I've always done in the past:

(setq visible-bell t)
(setq version-control t)
(setq make-backup-files t)
(setq backup-by-copying-when-mismatch t)
(setq kept-new-versions 30000)
(setq kept-old-versions 30000)

(setq-default indent-tabs-mode nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-c w") 'compare-windows)

(global-set-key (kbd "C-M-x") 'compile)

(global-set-key (kbd "C-x p") (lambda ()
				(interactive)  ; previous window
				(other-window -1)))

; Try a buncha stuff from https://github.com/flyingmachine/emacs-for-clojure/blob/master/init.el:

(unless (or (not (getenv "UNIXHOME"))  ; Perhaps running under sudo?
            (< emacs-major-version 24))
  (load (concat (getenv "UNIXHOME") "/components/emacs/craig/packages.el")))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Other stuff for use only when logged-in as 'craig' (or any account
;; that defines $UNIXHOME):
(when (getenv "UNIXHOME")
  (load (concat (getenv "UNIXHOME") "/components/emacs/craig/clojure.el"))
  (load (concat (getenv "UNIXHOME") "/components/emacs/craig/git.el")))

(load custom-file t)  ; No error if file doesn't exist.

;; End of my file.
