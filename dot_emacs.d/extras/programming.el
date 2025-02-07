;; buffer position in the fringe, instead of scroll bars
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)


(use-package  yaml-mode :mode ("\\yml$" . yaml-mode))

(use-package  json-mode
  :config
  (setq js-indent-level 2))

(use-package  display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))


;; Flycheck is a modern on-the-fly syntax checking extension for GNU
;; Emacs, intended as replacement for the older Flymake extension
;; which is part of GNU Emacs.
(use-package flycheck
  :straight t
  :blackout
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))
