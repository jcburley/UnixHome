;;; trimspace-mode.el --- minor mode to trim trailing whitespace and newlines

;; Copyright 2021  Björn Lindström <bkhl@elektrubadur.se>

;; Author: Björn Lindström <bkhl@elektrubadur.se>
;; URL: https://gitlab.com/bkhl/trimspace-mode
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: files, convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a very minimal minor mode that adds a hook to
;; run `delete-trailing-whitespace' before saving a file.
;;
;; It also has the function `trimspace-mode-unless-trailing-whitespace', which
;; activates the mode only if the buffer does not already have traling
;; whitespace or newlines.
;;
;; In addition, `require-final-newline' is enabled, since it's assumed that if
;; you want the editor to maintain trailing whitespace, you are most likely to
;; also want to maintain a trailing final newline in all files.

;;; Code:

(defun trimspace--trailing-whitespacep ()
  "Return t if the current buffer has any line with trailing whitespace.
Otherwise, return nil."
  (goto-char (point-min))
  (when (re-search-forward (rx blank eol) nil t) t))

(defun trimspace--trailing-newlinesp ()
    "Return t if current buffer has multiple newlines at the end of the file.
Otherwise, return nil."
    (goto-char (point-max))
    (when (re-search-backward (rx (= 2 (* blank) "\n") eot) nil t) t))

(defun trimspace--missing-final-newlinep ()
    "Return t if current buffer is missing a final newline.
Otherwise, return nil."
    (goto-char (point-max))
    (when (re-search-backward (rx not-newline eot) nil t) t))

;;;###autoload
(defun trimspace-mode-unless-trailing-whitespace ()
  "Start trimming trailing whitespace on save unless there is already some.

This is useful as a hook, to automatically trim whitespace on save, but skipping
files that have extraneous whitespace already when opening them."
  (interactive)
  (unless (save-restriction
            (widen)
            (save-excursion
              (or (and (trimspace--trailing-whitespacep)
                       (message "Buffer has trailing whitespace"))
                  (and (trimspace--trailing-newlinesp)
                       (message "Buffer has trailing newlines"))
                  (and (trimspace--missing-final-newlinep)
                       (message "Buffer lacks final newline")))))
    (trimspace-mode 1)))

;;;###autoload
(define-minor-mode trimspace-mode
  "When this minor mode is active, on saving a file, trim trailing whitespace
at the end of lines, and newlines at the end of the file."
  :init-value nil
  :lighter " Trim"
  :keymap nil

  (if trimspace-mode
      (progn
        (add-hook 'before-save-hook 'delete-trailing-whitespace 0 t)
        (unless require-final-newline
          (setq-local require-final-newline t)))
    (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
    (kill-local-variable require-final-newline)))

(provide 'trimspace-mode)

;;; trimspace-mode.el ends here
