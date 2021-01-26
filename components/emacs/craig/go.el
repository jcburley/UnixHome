;;; Tell (my old, dead fork of) the 'go generate' command to help next-error track source-file locations
(setenv "GO_EXEC_OUTPUT_STYLE" "gmake")

;;; Run 'go fmt' when saving a file in Go major more, but not if it's actually a template (.tmpl) file.
(defun maybe-gofmt-before-save ()
  "Run gofmt-before-save unless buffer name ends in \".tmpl\", indicating a template file."
  (unless (string-suffix-p ".tmpl" (buffer-name) t)
    (gofmt-before-save)))
(add-hook 'before-save-hook #'maybe-gofmt-before-save)
