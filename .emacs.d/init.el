;;; package --- my .emacs config
;;; Commentary:
;;  2017-10-19 jcburley copied some stuff from github.com/vidjuheffex/dotemacs/
;;; Code:

(message (concat load-file-name " loading!"))

;; Put customizations in per-hostname files.
(defvar system-specific-init-dir
 (concat "~/github/UnixHome/.emacs.d/systems/" (downcase (system-name)))
 "System-specific customization and other files are stored here.")
(make-directory system-specific-init-dir t)
(setq custom-file (concat system-specific-init-dir "/customizations.el"))

;; Confirm whether it's okay to exist without saving customizations.
(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

(setq user-full-name "James Craig Burley")
(setq user-mail-address "james@burleyarch.com")
(setq global-mark-ring-max 200)

; Try a buncha stuff from https://github.com/flyingmachine/emacs-for-clojure/blob/master/init.el:

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(defun package-stamp-write-file (s f)
  "Write given package stamp to a specified file."
  (write-region s nil f))

(defvar package-stamp
      (concat
       (format-time-string "%Y-%m-%d\n")
       (prin1-to-string package-archives))
      "String containing today's date on first line followed by stringized var package-archives.")

(defvar package-stamp-file
  "~/.emacs.d/package-refresh.STAMP"
  "Name of file in which package stamp is written -- local to each machine.")

(defun read-file-contents (f)
  "Read contents of a file and return the result as a string."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun package-stamp-check-file (s f)
  "Return t if file exists and contains the stamp, nil otherwise."
  (and (file-exists-p f)
       (string= s (read-file-contents f))))

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(unless (or package-archive-contents
            (package-stamp-check-file package-stamp package-stamp-file))
  (package-refresh-contents)
  (package-stamp-write-file package-stamp package-stamp-file))

(package-initialize)

(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; company-mode
    company
))

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

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

; Bring in use-package:

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(package-initialize)

; Stuff I've always done in the past:

(setq visible-bell t)
(setq version-control t)
(setq-default indent-tabs-mode nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-c w") 'compare-windows)

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
