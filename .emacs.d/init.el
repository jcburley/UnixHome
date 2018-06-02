;;; package --- my .emacs config
;;; Commentary:
;;  2017-10-19 jcburley copied some stuff from github.com/vidjuheffex/dotemacs/
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(defvar system-specific-scratch-dir
 (concat "~/.emacs.d/systems/" (downcase (system-name)))
 "System-specific scratch, cache, and other temporary files are stored here.")
(make-directory system-specific-scratch-dir t)

(defvar system-specific-init-dir
 (concat "~/github/UnixHome/.emacs.d/systems/" (downcase (system-name)))
 "System-specific customization and other files are stored here.")
(make-directory system-specific-init-dir t)

;; Put customizations in per-hostname files.
(setq custom-file (concat system-specific-init-dir "/customizations.el"))

;; Confirm whether it's okay to exit without saving customizations.
;; (If running a version of Emacs that does not support this, you
;; might have to use ESC-x kill-emacs to exit.)
(unless (< emacs-major-version 25)
  (add-hook 'kill-emacs-query-functions
	    'custom-prompt-customize-unsaved-options))

(setq user-full-name "James Craig Burley")
(setq user-mail-address "james@burleyarch.com")
(setq global-mark-ring-max 200)

; If not already in keymap and/or bound, add ability to "git push" to
; keymap, bound to 'vc-push, and to "Tools | Version Control" menu.
(if (not (fboundp 'vc-push))
    (defun vc-push ()
      "Do a git push"
      (interactive)
      (async-shell-command "git push")))
(when (not (fboundp (global-key-binding (kbd "C-x v P"))))
  (global-set-key (kbd "C-x v P") 'vc-push)
  (define-key-after
    (lookup-key global-map [menu-bar tools vc])
    [vc-push]
    '("Push to upstream" . vc-push)
    'vc-revert))

; Find git and git bash.
(when (memq system-type '(windows-nt ms-dos))
  (add-to-list 'exec-path "/Program Files/git/bin")
  (setenv "PATH" (mapconcat #'identity exec-path path-separator)))

; Stuff I've always done in the past:

(setq visible-bell t)
(setq version-control t)
(setq make-backup-files t)
(setq backup-by-copying-when-mismatch t)
(setq kept-new-versions 30000)
(setq kept-old-versions 30000)

(setq-default indent-tabs-mode nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-c w") 'compare-windows)

; Try a buncha stuff from https://github.com/flyingmachine/emacs-for-clojure/blob/master/init.el:

(unless (< emacs-major-version 24)
  (load (concat (getenv "UNIXHOME") "/.emacs.d/craig/packages.el")))

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

; Clojure stuff:

; Display Clojure function signatures in minibuffer while typing in REPL:
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
; Set result prefix for REPL:
(setq cider-repl-result-prefix ";; => ")
; Use company-mode for completion (very nice!):
(if (boundp 'global-company-mode)
    (global-company-mode t))
; Try new-ish feature that supports TAB completion or manual indent:
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
; Enable fuzzy candidate matching for company-mode:
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
; Don't auto-select *cider-error* on error:
(setq cider-auto-select-error-buffer nil)

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; provides minibuffer documentation for the code you're typing into the repl
; See for why not 'cider-turn-on-eldoc-mode: https://github.com/clojure-emacs/cider/issues/934
(add-hook 'cider-mode-hook 'eldoc-mode)

;; I've been typing this accidentally too often, and it typically
;; wedges my session (it's bound to cider-eval-region, and the region
;; is typically quite large and includes many expressions).
(add-hook 'cider-repl-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c C-r"))))

;Use these only when working on clojure-mode itself:
;(load-file "~/clojure/clojure-mode/clojure-mode.el")
;(load-file "~/clojure/clojure-mode/clojure-mode-extra-font-locking.el")

; End Clojure stuff.


(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-M-x") 'compile)

(global-set-key (kbd "C-x p") (lambda ()
				(interactive)  ; previous window
				(other-window -1)))

(load custom-file t)  ; No error if file doesn't exist.

;; End of my file.
