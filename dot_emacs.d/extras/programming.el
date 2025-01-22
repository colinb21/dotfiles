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

;; buffer position in the fringe, instead of scroll bars
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)


(use-package  yaml-mode :mode ("\\yml$" . yaml-mode))

(use-package  blacken
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-line-length '88))

(use-package  json-mode
  :config
  (setq js-indent-level 2))

(use-package  display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

;; from https://www.mclare.blog/posts/using-uv-in-emacs/
(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (venv-path (expand-file-name ".venv" project-root))
         (python-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "Scripts/python.exe"
                         "bin/python")
                       venv-path)))
    (if (file-exists-p python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter python-path)

          ;; Update exec-path to include the venv's bin directory
          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" project-root))))

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
