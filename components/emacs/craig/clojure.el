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

(add-to-list 'auto-mode-alist '("\\.joke\\'" . clojure-mode))
