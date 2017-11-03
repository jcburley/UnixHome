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

(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-M-x") 'compile)

(global-set-key (kbd "C-x p") (lambda ()
				(interactive)  ; previous window
				(other-window -1)))

(load-file "~/clojure/clojure-mode/clojure-mode.el")
(load-file "~/clojure/clojure-mode/clojure-mode-extra-font-locking.el")
