(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives
               (cons "melpa" url) t))
(add-to-list 'package-archives
             '("tromey2" . "http://tromey.com/elpa/") t)
;; Adding a final, unnecessary, instance of tromey, because GNU Emacs seems to have a bug
;; that causes a 'Failed to download...' message to appear in *Messages* for the final
;; library in the list. See: https://github.com/melpa/melpa/issues/5755
(add-to-list 'package-archives
             '("tromey" . "https://tromey.com/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(defvar package-stamp
  (concat
   (format-time-string "%Y-%m-%d\n")
   (prin1-to-string package-archives))
  "String containing today's date on first line followed by stringized var package-archives.")

(defvar package-stamp-file
  (concat session-specific-scratch-dir "/package-refresh.STAMP")
  "Name of file in which package stamp is written -- local to each machine.")

(defun read-file-contents (f)
  "Read contents of a file and return the result as a string."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun write-file-contents (s f)
  "Write given string to a specified file."
  (write-region s nil f))

(defun file-contents-= (s f)
  "Return t if file exists and contains exactly the string, nil otherwise."
  (and (file-exists-p f)
       (string= s (read-file-contents f))))

(message (concat load-file-name " might call package-refresh-contents for: " package-stamp))

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(unless (or package-archive-contents
            (file-contents-= package-stamp package-stamp-file))
  (package-refresh-contents)
  (write-file-contents package-stamp package-stamp-file))

(message (concat load-file-name " about to call package-initialize!"))

(package-initialize)

(message (concat load-file-name " after call to package-initialize!"))

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

(when (and (fboundp 'exec-path-from-shell-initialized)
           (memq window-system '(mac ns x)))
  (exec-path-from-shell-initialize))

                                        ; Bring in use-package:

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(package-initialize)
