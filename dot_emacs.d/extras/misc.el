;; all the things too small for their own file, but too big to be in
;; init.el and which don't have a general theme of their own.

;; which-key is a minor mode for Emacs that displays the key indings
;; following your currently entered incomplete command (a prefix) in a
;; popup.
(use-package  which-key
  :blackout
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode)
  :bind* (("C-z ?" . which-key-show-top-level))
  :config
  (progn (which-key-mode t)
	 (which-key-add-key-based-replacements "C-z ?" "top level bindings")))

(use-package  comment-dwim-2  :bind (("M-;" . comment-dwim-2)))

(use-package  persistent-scratch
  :config (persistent-scratch-setup-default))

;; ripgrep search
(use-package  rg)

;; When you visit a file, point goes to the last place where it was
;; when you previously visited the same file.
(use-package  saveplace
  :init (save-place-mode 1)
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-limit nil)))

;; psession restores sessions, including dired from the last time we
;; ran emacs. But on MacOS ls support --dired so a warning will appear
;; unless I tell dired not to look for the Linux option
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(use-package easysession
  :ensure t
  :custom
  ;; Interval between automatic session saves
  (easysession-save-interval (* 10 60))
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103))

;;Vundo (visual undo) displays the undo history as a tree and lets you
;;move in the tree to go back to previous buffer states. To use
;;vundo,type M-x vundo RET in the buffer you want to undo. An undo
;;tree buffer should pop up.
(use-package vundo)


;; C-w becomes backspace if no region active. If a region is active,
;; delete it.
(defun vz/backward-delete-or-kill-region (arg)
  "Run `kill-region' if region is active or
`backward-delete-char'."
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (if (bound-and-true-p paredit-mode)
        (paredit-backward-delete arg)
      (backward-delete-char arg))))
(global-set-key (kbd "C-w") 'vz/backward-delete-or-kill-region)

;; forward-sentence and org-forward-sentence (both M-e by default)
;; seem spectacularly useless at detecting the end of sentences in
;; English. Try this instead.
(use-package  sentence-navigation
  :defer t)
(global-set-key (kbd "M-e") 'sentence-nav-forward)
(global-set-key (kbd "M-a") 'sentence-nav-backward)

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)))

;; Helpful is an alternative to the built-in Emacs help that provides
;; much more contextual information.
(use-package helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;; Replace highlighted text with what I type rather than just
;; inserting at point.
(delete-selection-mode t)

;;This package can show available bindings in the current buffer.
(use-package free-keys
  :commands free-keys)

;; see http://www.dr-qubit.org/undo-tree/undo-tree.txt for instructions - C-x u is the shit
(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; Prevent undo tree files from polluting the file systems
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

