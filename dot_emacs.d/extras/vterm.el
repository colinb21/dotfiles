(use-package  vterm
  :init (add-hook 'vterm-mode-hook 'hide-mode-line-mode))

;; M-x vterm-toggle will create a vterm if there isn't one, and then
;; display it. Repeating the command will restore the previous window
;; arrangement.
(use-package  vterm-toggle
  :straight t
  :bind ("C-c t" . vterm-toggle))

