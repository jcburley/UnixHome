;; Disable mouse on typing
(defvar jogo3000/mouse-disabled nil)
(use-package disable-mouse)

(defun jogo3000/disable-mouse ()
  "Disable mouse."
  (setq jogo3000/mouse-disabled t)
  (global-disable-mouse-mode))

(defun jogo3000/enable-mouse ()
  "Enable mouse."
  (global-disable-mouse-mode -1)
  (setq jogo3000/mouse-disabled nil))

(defun jogo3000/temporarily-disable-mouse ()
  "Temporarily disable mouse."
  (unless jogo3000/mouse-disabled
    (jogo3000/disable-mouse)
    (run-with-timer 3 nil 'jogo3000/enable-mouse)))

(defun jogo3000/disable-touchpad-mode ()
  "Make it so that touchpad is inactive when I type."
  (interactive)
  (add-to-list 'post-self-insert-hook 'jogo3000/temporarily-disable-mouse))
