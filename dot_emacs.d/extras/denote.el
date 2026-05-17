;; Denote
(use-package  denote
  :init
  (denote-rename-buffer-mode t)
  :custom
  (denote-directory (getenv "DENOTE_DIR"))
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind
  (("C-c n n" . denote-create-note)
   ("C-c n d" . denote-date)
   ("C-c n i" . denote-link-or-create)
   ("C-c n l" . denote-find-link)
   ("C-c n b" . denote-find-backlink)
   ("C-c n d" . denote-org-dblock-insert-links)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n k" . denote-keywords-add)
   ("C-c n K" . denote-keywords-remove)
   ("C-c n j" . denote-journal-new-or-existing-entry)))

(use-package denote-org
  :ensure t
  :commands
  ;; I list the commands here so that you can discover them more
  ;; easily.  You might want to bind the most frequently used ones to
  ;; the `org-mode-map'.
  ( denote-org-link-to-heading
    denote-org-backlinks-for-heading

    denote-org-extract-org-subtree

    denote-org-convert-links-to-file-type
    denote-org-convert-links-to-denote-type

    denote-org-dblock-insert-files
    denote-org-dblock-insert-links
    denote-org-dblock-insert-backlinks
    denote-org-dblock-insert-missing-links
    denote-org-dblock-insert-files-as-headings))

(use-package denote-journal
  :ensure t
  ;; Bind those to some key for your convenience.
  :commands ( denote-journal-new-entry
              denote-journal-new-or-existing-entry
              denote-journal-link-or-create-entry )
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

(use-package denote-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           denote meeting notes
;;
;; from https://protesilaos.com/codelog/2024-09-20-emacs-use-denote-for-meetings-events/
;;

(defvar my-denote-colleagues (split-string (getenv "MY_DENOTE_COLLEAGUES"))
  "List of names I collaborate with.
There is at least one file in the variable `denote-directory' that has
the name of this person.")

(defvar my-denote-colleagues-prompt-history nil
  "Minibuffer history for `my-denote-colleagues-new-meeting'.")

(defun my-denote-colleagues-prompt ()
  "Prompt with completion for a name among `my-denote-colleagues'.
Use the last input as the default value."
  (let ((default-value (car my-denote-colleagues-prompt-history)))
    (completing-read
     (format-prompt "New meeting with COLLEAGUE" default-value)
     my-denote-colleagues
     nil :require-match nil
     'my-denote-colleagues-prompt-history
     default-value)))

(defun my-denote-colleagues-get-file (name)
  "Find file in variable `denote-directory' for NAME colleague.
If there are more than one files, prompt with completion for one among
them.

NAME is one among `my-denote-colleagues'."
  (if-let ((files (denote-directory-files name))
           (length-of-files (length files)))
      (cond
       ((= length-of-files 1)
        (car files))
       ((> length-of-files 1)
        (completing-read "Select a file: " files nil :require-match)))
    (user-error "No files for colleague with name `%s'" name)))

(defun my-denote-colleagues-new-meeting ()
  "Prompt for the name of a colleague and insert a timestamped heading therein.
The name of a colleague corresponds to at least one file in the variable
`denote-directory'.  In case there are multiple files, prompt to choose
one among them and operate therein.

Names are defined in `my-denote-colleagues'."
  (declare (interactive-only t))
  (interactive)
  (let* ((name (my-denote-colleagues-prompt))
         (file (my-denote-colleagues-get-file name))
         (time (format-time-string "%F %a %R")))  ; remove %R if you do not want the time
    (with-current-buffer (find-file file)
      (goto-char (point-max))
      ;; Here I am assuming we are in `org-mode', hence the leading
      ;; asterisk for the heading.  Adapt accordingly.
      (insert (format "* [%s]\n\n" time)))))
;;
;; end    denote meeting notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Denote extensions
;;
;; consult-notes view the denote directory with completion AND (!!!)
;; preview window AND you don't have to complete your way to the
;; denote directory
(use-package  consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d ,(getenv "DENOTE_DIR"))))
  :bind
  (("C-c n f" . consult-notes)
   ("C-c n s" . consult-notes-search-in-all-notes)))

;; denote stores stuff in the private disk. If it isn't mounted when
;; emacs starts, try to fix that.
(let* ((private-disk (getenv "PRIVATE_DISK"))
       (info-cmd (concat "hdiutil info | grep " private-disk ))
       (already-mounted (shell-command info-cmd nil nil)))
  (if (= already-mounted 1)
      (let ((attach-cmd (concat "hdiutil attach " private-disk)))
	(shell-command attach-cmd nil nil))))


(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))
