(use-package  magit
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (defun inkel/magit-log-edit-mode-hook ()
      (setq fill-column 72)
      (turn-on-auto-fill))
    (add-hook 'magit-log-edit-mode-hook 'inkel/magit-log-edit-mode-hook)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))))

