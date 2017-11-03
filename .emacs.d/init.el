;;; package --- my .emacs config
;;; Commentary:
;;  2017-10-19 jcburley copied some stuff from github.com/vidjuheffex/dotemacs/
;;; Code:
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq visible-bell t)
(setq version-control t)

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

; Display Clojure function signatures in minibuffer while typing in REPL:
(add-hook 'clojure-mode-hook #'eldoc-mode)
; Display Clojure function signatures in minibuffer while typing in REPL:
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-M-x") 'compile)

(global-set-key (kbd "C-x p") (lambda ()
				(interactive)  ; previous window
				(other-window -1)))

;Use these only when working on clojure-mode itself:
;(load-file "~/clojure/clojure-mode/clojure-mode.el")
;(load-file "~/clojure/clojure-mode/clojure-mode-extra-font-locking.el")
