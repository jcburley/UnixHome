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
