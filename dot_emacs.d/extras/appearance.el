;; git diffs in the fringe
(use-package  git-gutter-fringe
  :if (window-system)
  :config (global-git-gutter-mode +1))

;; buffer position in the fringe, instead of scroll bars
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; Goggles highlights the modified region using pulse. Currently the
;; commands undo, yank, kill and delete are supported.
(use-package  goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;; highlight trailing whitespace
(setq-default show-trailing-whitespace t)
;; but not in shell mode
(defun my-hide-trailing-whitespace-maybe ()
  "Disable `show-trailing-whitespace' in selected modes."
  (when (derived-mode-p 'shell-mode
                        'term-mode)
    (setq show-trailing-whitespace nil)))

(add-hook 'after-change-major-mode-hook
          'my-hide-trailing-whitespace-maybe)

;; display the current column
(column-number-mode)

;; shift-LeftArrow moves to window-left, shift-UpArrow moves to...
(windmove-default-keybindings)

(use-package  expand-region :bind ("C-=" . er/expand-region))

(global-prettify-symbols-mode)

;;Show matching parenthesis context when offscreen
(setopt show-paren-context-when-offscreen 'overlay)

(show-paren-mode 1)

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area.  Has no effect if the character before point is not of the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))


(use-package  smartparens
  :blackout t
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq smartparens-global-mode t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p)))
  :hook
  (python-mode . smartparens-mode)
  (c++-mode . smartparens-mode)
  (lisp-interaction-mode . smartparens-mode)
  (emacs-lisp-mode . smartparens-mode)
  (LaTeX-mode . smartparens-mode)
  (org-mode . smartparens-mode)
  (rust-mode . smartparens-mode)
  :bind
  ;; (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
  ;;  ("C-M-f" . sp-forward-sexp)
  ;;  ("C-M-b" . sp-backward-sexp)
  ;;  ("C-M-n" . sp-up-sexp)
  ;;  ("C-M-d" . sp-down-sexp)
  ;;  ("C-M-u" . sp-backward-up-sexp)
  ;;  ("C-M-p" . sp-backward-down-sexp)
  ;;  ("C-M-w" . sp-copy-sexp)
  ;;  ("M-s" . sp-splice-sexp)
  ;;  ("M-r" . sp-splice-sexp-killing-around)
  ;;  ("C-)" . sp-forward-slurp-sexp)
  ;;  ("C-}" . sp-forward-barf-sexp)
  ;;  ("C-(" . sp-backward-slurp-sexp)
  ;;  ("C-{" . sp-backward-barf-sexp)
  ;;  ("M-S" . sp-split-sexp)
  ;;  ("M-J" . sp-join-sexp)
  ;;  ("C-M-t" . sp-transpose-sexp))
  )

;; the following code makes M-x shell windows behave well across
;; resize operations. Without them, changes to a window's width
;; produce ugliness
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))

;; add this hook as buffer local, so it runs once per window.
(defun my-shell-mode-hook ()
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)


;; Whenever the window scrolls a light will shine on top of your cursor so
;; you know where it is.
(use-package  beacon
  :config
  (setq beacon-color "DarkOrange2"
        beacon-blink-duration 1)
  (beacon-mode t))

(use-package  ibuffer
  :init
    :bind (("C-x C-b" . ibuffer)))

(setq ibuffer-saved-filter-groups
	  (quote (("default"
		   ("dired" (mode . dired-mode))
		   ("org" (mode . org-mode))
		    ("magit" (name . "^magit"))
		   ("planner" (or
				(name . "^\\*Calendar\\*$")
				(name . "^\\*Org Agenda\\*")))
           ("Programming" (or
                           (mode . python-ts-mode)
                           (mode . c-ts-mode)
                           (mode . c++-ts-mode)
                           (mode . emacs-lisp-mode)))
		   ("emacs" (or
			     (name . "^\\*scratch\\*$")
			     (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

(use-package   auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package  rainbow-delimiters :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package hide-mode-line)

(use-package  all-the-icons)

(use-package  all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Support for ANSI escape color codes in emacs compilation buffer
;; Fixes build and test execution output in LSP and DAP
(defun colorize-compilation-buffer ()
  (when (derived-mode-p 'compilation-mode)
    (ansi-color-process-output nil)
    (setq-local comint-last-output-start (point-marker))))

(use-package  ansi-color
  :config
  (add-hook 'compilation-filter-hook
            #'colorize-compilation-buffer))


(setq visible-bell 1)

(setq ring-bell-function 
      (lambda ()
	    (unless (memq this-command
		              '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	      (ding))))


(setq case-fold-search t) ;; makes search case insensitive

(global-hl-line-mode 1)
