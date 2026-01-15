
;; Generate d2 diagrams using org-mode and org-babel.
(use-package ob-d2 :straight t)

(use-package  org
  :straight (:type built-in) ;; force use in built-in version rather than installing newest
  :after denote
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib ;; needed for org-tempo
  :config
  (progn
    ;; config stuff
    (setq-default org-src-fontify-natively t)

    ;; stop babel asking me if I really want to evaluate a snippet
    (defun my-org-confirm-babel-evaluate (lang body)
      (not (or (string= lang "ditaa")
	       (string= lang "C++")
	       (string= lang "haskell")
               (string= lang "d2"))))
    (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
    (setopt org-directory (getenv "ORG_DIR"))


    ;; variable-pitch as opposed to fixed width characters visual line
    ;; "ong lines will flow and adjust to the width of the window. "
    (add-hook 'org-mode-hook
	      (lambda ()
		(variable-pitch-mode)
		(visual-line-mode)))
    ;; org markup for things surrounded by asterisks!
    (add-to-list 'org-emphasis-alist
		 '("*" (:foreground "red")
		   ))
    (add-to-list 'org-modules 'org-tempo))
  :custom
  (org-agenda-window-setup 'current-window)
  (org-babel-load-languages
   '((d2 . t)(ditaa . t) (scheme . t) (C . t) (emacs-lisp . t)
     (python . t) (shell . t) (haskell . t) (dot . t) (octave . t)))
  (org-capture-templates
   '(
     ("t" "Todo" entry (file+headline "/Volumes/Personal/org/ToDo.org" "Tasks")
      "* TODO %?\n  %i\n  %a")
     ("note"
      "Note (Org Protocol)"
      entry
      (file "~/org/notes.org")
      (function (lambda ()
                  (string-join
                   '("* %U"
                     "%i")
                   "\n")))
      :prepend t
      :immediate-finish t
      :empty-lines-after 1)
     ;; key "t" description "todo" type "entry - an org mode entry
     ;; with a headline, child of the target"
     ;; ("t" "todo" entry
     ;;  ;; target "file+headline <PATH> "Tasks" - in the file, under a heading "Tasks"
     ;;  (file+headline "~/.emacs.d/org-todo/ToDo.org" "Tasks")
     ;;  ;; template "* TODO [#A] " %? -- after completing template, put point here
     ;;  "* TODO [#A] %?")
     ;; ("j" "journal" entry
     ;;  (file+olp+datetree "~/.emacs.d/org-todo/Journal.org")
     ;;  "* %?" :empty-lines 1)
     ;; ("f" "Fleeting note" item
     ;;  (file+headline org-default-notes-file "Notes")
     ;;  "- %?")
      ;;     ("p" "Permanent note" plain
      ;; (file denote-last-path)
      ;; #'denote-org-capture
      ;; :no-save t
      ;; :immediate-finish nil
      ;; :kill-buffer t
      ;; :jump-to-captured t)
))
  (org-default-priority 65)
  (org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-highest-priority 65)
  (org-log-done t)
  (org-lowest-priority 67)
  (org-priority-faces'((65 :foreground "#F0DFAF" :weight bold)
		       (66 :foreground "LightSteelBlue")
		       (67 :foreground "OliveDrab")))
  (org-startup-folded 'overview)
  (org-startup-indented t))

(use-package  org-mac-link
  :bind ("C-c g" . org-mac-link-get-link))

;; :bind
;; (:map org-mode-map ("<M-return>" . 'org-insert-heading))
(global-set-key (kbd "M-<return>") 'org-insert-heading)


;; Then, we set up a font-lock substitution for list markers (I always
;; use “-” for lists, but you can change this if you want) by
;; replacing them with a centered-dot character:
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; useful because we've set visual-line-mode in org-mode, which
;; replaces paragraph filling for long lines. From
;;
;; https://stackoverflow.com/questions/1416171/emacs-visual-line-mode-and-fill-paragraph
(defun maybe-fill-paragraph (&optional justify region)
  "Fill paragraph at or after point (see `fill-paragraph').

Does nothing if `visual-line-mode' is on."
  (interactive (progn
         (barf-if-buffer-read-only)
         (list (if current-prefix-arg 'full) t)))
  (or visual-line-mode
      (fill-paragraph justify region)))

;; Replace M-q with new binding:
(global-set-key "\M-q" 'maybe-fill-paragraph)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; nicer text centring and line breaks
(use-package olivetti
  :config (add-hook 'org-mode-hook 'olivetti-mode))

;; by itself C-c c creates a new capture buffer. Prefixed with C-u it
;; takes you to the set of all such capture buffers.
(define-key global-map (kbd "C-c c") 'org-capture)

(use-package  org-superstar
  :straight t
  :config
  (progn
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))))

(setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?•)))

(use-package  org-present :straight t)

;Insert new headlines after current subtree.
(setopt org-insert-heading-respect-content t)


;; stolen from timu-func-make-capture-frame
(use-package noflet)
(defun cb/func-make-capture-frame ()
  "Create a new frame and run `org-capture'."
  (interactive)
  (make-frame '((name . "capture")
                (top . 300)
                (left . 700)
                (width . 80)
                (height . 25)))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(use-package org-modern)
