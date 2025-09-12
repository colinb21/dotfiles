(use-package chezmoi)

(defun chezmoi-wrap-find-file (file)
  "If a file is managed via chezmoi, load the source template, otherwise use find-file"
  (interactive "FFind file: ")
  (if (member file (chezmoi-managed-files))
      (chezmoi-find file)
    (find-file file)))

(global-set-key (kbd "C-x C-f") #'chezmoi-wrap-find-file)

(defun chezmoi-git-sync()
  "Git autocommit hook for Chezmoi"
    (if chezmoi-mode
        (let ((target (string-trim (shell-command-to-string
                                    (format "chezmoi target-path %s" buffer-file-name)))))
          (if (not (= 0 (shell-command
                         (format "chezmoi git commit -- -m 'updates to %s' %s"
                                 target buffer-file-name))))
              (user-error "Error commiting changes"))
          (if (not (= 0 (shell-command "chezmoi git push")))
              (user-error "Error pushing changes")))))

(add-hook 'after-save-hook #'chezmoi-git-sync)

(defun chezmoi-add-file()
  (interactive)
  (if (member buffer-file-name (chezmoi-managed-files))
      (user-error (format "File %s is already managed by Chezmoi" buffer-file-name))
    (if (not (= 0 (shell-command (format "chezmoi add %s" buffer-file-name))))
          (user-error "Error adding file to Chezmoi"))))

(defun chezmoi-update-template-mode()
  (interactive)
  (add-hook 'after-save-hook #'chezmoi-update-template nil t))

(defun chezmoi-update-template()
  (interactive)
  (if (not (= 0 (shell-command "chezmoi init")))
      (user-error "Error updating template"))
  (if (not (= 0 (shell-command
                 (format "chezmoi git commit -- -m 'updates to %s' %s"
                         buffer-file-name buffer-file-name))))
      (user-error "Error commiting changes"))
  (if (not (= 0 (shell-command "chezmoi git push")))
      (user-error "Error pushing changes")))
