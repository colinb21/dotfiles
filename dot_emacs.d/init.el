;; Non-nil means enter debugger if an error is signaled.
(setq debug-on-error t)


;; Start a timer.  Check when finished init.
(defconst emacs-start-time (current-time))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t)
  )

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;;By default, auto-save files are stored in the current directory with
;;a file name on the form #file#. If you don’t want to clutter up your
;;file tree with Emacs’ backup files, you can save them to a dedicated
;;directory:
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup-files"))))

;; update packages every 4 days
(use-package auto-package-update :ensure t
  :config (auto-package-update-maybe))


(use-package no-littering)

(use-package exec-path-from-shell
  ;;  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PRIVATE_DISK" "DENOTE_DIR" "ORG_DIR" "MY_DENOTE_COLLEAGUES"))
  (exec-path-from-shell-initialize))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ns-command-modifier 'meta)
 '(org-agenda-files (list (expand-file-name "ToDo.org" (getenv "ORG_DIR"))) nil nil "Customized with use-package org")
 '(warning-suppress-types '((use-package) (comp))))

;; nicer chezmoi diff etc
(use-package chezmoi)
(global-set-key (kbd "C-c C f")  #'chezmoi-find)
(global-set-key (kbd "C-c C s")  #'chezmoi-write)

;; Blackout is a package which allows you to hide or customize the
;; display of major and minor modes in the mode line.
(use-package blackout)

(which-function-mode)

(server-start)

;; accept y or n instead of demanding yes or no every time
(setopt use-short-answers t)

;; winner-mode lets you move back and forth through your window
;; configuration history with C-c <LEFT> and C-c <right>.
(winner-mode 1)

;;general.el provides a more convenient method for binding keys in
;;emacs (for both evil and non-evil users). Like use-package, which
;;provides a convenient, unified interface for managing packages,
;;general.el is intended to provide a convenient, unified interface
;;for key definitions.
(use-package general)

(use-package nerd-icons)

(load-file (expand-file-name "extras/spelling.el" user-emacs-directory))
(load-file (expand-file-name "extras/load-consult.el" user-emacs-directory))
(load-file (expand-file-name "extras/completion.el" user-emacs-directory))
(load-file (expand-file-name "extras/denote.el" user-emacs-directory))
(load-file (expand-file-name "extras/org.el" user-emacs-directory))
(load-file (expand-file-name "extras/appearance.el" user-emacs-directory))
(load-file (expand-file-name "extras/misc.el" user-emacs-directory))
(load-file (expand-file-name "extras/programming.el" user-emacs-directory))
(load-file (expand-file-name "extras/python.el" user-emacs-directory))
(load-file (expand-file-name "extras/magit.el" user-emacs-directory))
(load-file (expand-file-name "extras/vterm.el" user-emacs-directory))
;;(load-file (expand-file-name "extras/lsp.el" user-emacs-directory))
(load-file (expand-file-name "extras/treesit.el" user-emacs-directory))
(load-file (expand-file-name "extras/rust.el" user-emacs-directory))
(load-file (expand-file-name "extras/nano.el" user-emacs-directory))
(load-file (expand-file-name "extras/gptel.el" user-emacs-directory))
(load-file (expand-file-name "extras/casual.el" user-emacs-directory))
(load-file (expand-file-name "extras/embark.el" user-emacs-directory))
(load-file (expand-file-name "extras/snippets.el" user-emacs-directory))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package python
  :config
  (require 'eglot)
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-ts-mode-hook #'flymake-mode)
  ;; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) "ruff-lsp"))
  )

(use-package eglot
  :bind (("C-c e c" . eglot-reconnect)
         ("C-c e d" . flymake-show-buffer-diagnostics)
         ("C-c e f f" . eglot-format)
         ("C-c e f b" . eglot-format-buffer)
         ("C-c e l" . eglot)
         ("C-c e r n" . eglot-rename)
         ("C-c e s" . eglot-shutdown)))

(straight-use-package 'flymake-ruff)
(add-hook 'python-mode-hook #'flymake-ruff-load)

