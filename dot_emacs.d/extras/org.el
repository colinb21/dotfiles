
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
    (defun my/org-capture-web (url title body)
      "Capture a web snippet. Intended to be invoked via emacsclient -e."
      (org-link-store-props :type "http"
                            :link url
                            :description title
                            :annotation (org-link-make-string url title))
      (let ((org-capture-link-is-already-stored t)
            (org-capture-initial body))
        (org-capture nil "w")))
    (defun my/org-confirm-babel-evaluate (lang body)
      (not (or (string= lang "ditaa")
	           (string= lang "C++")
	           (string= lang "haskell")
               (string= lang "d2"))))
    (setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)
    (setopt org-directory (getenv "ORG_DIR"))
    (setq org-log-into-drawer t)


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
   `(
     ("t" "Todo" entry (file+headline "ToDo.org" "Tasks") "* TODO %?\n  %i\n  %a")
     ("w" "Web snippet" entry
      (file ,(expand-file-name "google-doc-captures.org" (getenv "DENOTE_DIR")))
      ;;      "* Google doc %i"
      "* %:description :web:\n:PROPERTIES:\n:URL: %:link\n:CAPTURED: %U\n:END:\n\n#+begin_quote\n%i\n#+end_quote\n\n%?"
      ;;      "* LINK %:description\n :web:\n %:link\n%i  %t\n"
      ;;      :immediate-finish t
      )))
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
(bind-key "M-<return>" 'org-insert-heading)

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
(bind-key "\M-q" 'maybe-fill-paragraph)

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c a" 'org-agenda)

;; nicer text centring and line breaks
(use-package olivetti
  :config
  (add-hook 'org-mode-hook 'olivetti-mode)
  (setq olivetti-body-width 0.8))

;; by itself C-c c creates a new capture buffer. Prefixed with C-u it
;; takes you to the set of all such capture buffers.
(bind-key "C-c c" 'org-capture)

(use-package org-superstar
    :config
    (setq org-superstar-leading-bullet " ")
    (setq org-superstar-headline-bullets-list '("①" "②" "③" "④" "⑤" "⑥" "⑦" "⑧" "⑨" "⑩"))

    (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
    (setq org-superstar-todo-bullet-alist '(("TODO"  . 9744)
                                            ("DONE"  . 9745)))
    :hook (org-mode . org-superstar-mode))

(use-package  org-present :straight t)

;Insert new headlines after current subtree.
(setopt org-insert-heading-respect-content t)


;; stolen from timu-func-make-capture-frame
;;(use-package noflet)
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

;; (add-to-list 'org-capture-templates
;;              `("l" "Link" entry
;;                (file ,(expand-file-name "file-foo" (getenv "DENOTE_DIR")))
;;                "* %:description :web:\n:URL: %:link\n:CAPTURED: %U\n\n\n#+begin_quote\n%i\n#+end_quote\n\n%?"
;;                :immediate-finish t))

(use-package org-bullets
  :after org
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :after org
  :ensure t
  :hook ((org-mode . org-modern-mode))
  :custom
  (org-modern-block-fringe nil)
  (org-modern-table nil)
  (org-modern-star nil))

;; from https://news.ycombinator.com/item?id=48449187
(defun my/org-to-markdown-clipboard ()
  "Export org region (or buffer) to Markdown and copy to clipboard.
  With no active region, exports the whole buffer."
  (interactive)
  (require 'ox-md)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (md (org-export-string-as text 'md t '(:with-toc nil
                                                          :with-author nil
                                                          :with-date nil
                                                          :with-title nil))))
    (kill-new md)
    (message "Markdown copied (%d chars)" (length md))))

(defun my/box-table-to-org (beg end)
  "Convert a Unicode box-drawing table in region BEG..END into an org-mode table.
Handles tables using ┌ ┬ ┐ ┼ ├ ┤ └ ┴ ┘ │ ─ characters, as commonly
pretty-printed by terminals from markdown tables (e.g. Claude Code output)."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (lines (split-string text "\n"))
         (result
          (delete
           nil
           (mapcar
            (lambda (line)
              (cond
               ;; Top or bottom border line: only border-drawing chars/whitespace
               ;; and no interior "cross" needed — drop entirely.
               ((string-match-p "\\`[[:space:]]*[┌└][─┬┴┐┘]*[[:space:]]*\\'" line)
                nil)
               ;; Separator line (has ├ or ┼ or ┤ made of ─): becomes a bare rule.
               ((string-match-p "[├┼┤]" line)
                "|-")
               ;; Content line: convert │ to | and trim.
               ((string-match-p "│" line)
                (string-trim (replace-regexp-in-string "│" "|" line)))
               ;; Anything else (blank lines etc.) pass through unchanged.
               (t (if (string-blank-p line) nil line))))
            lines))))
    (delete-region beg end)
    (goto-char beg)
    (insert (mapconcat #'identity result "\n") "\n")
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        (when (re-search-forward "|" end t)
          (org-table-align))))))
