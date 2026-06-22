;;; denote-jira.el --- Create Denote task notes from JIRA work items -*- lexical-binding: t; -*-

;; Create a Denote task note from a JIRA work item.  The issue summary
;; is fetched with the Atlassian CLI (acli), which must already be
;; authenticated.  Issues and PRs are *not* created from here.
;;
;; Load from init.el alongside extras/denote.el, e.g.:
;;   (load-file (expand-file-name "extras/denote-jira.el" user-emacs-directory))

;;; Code:

(require 'denote)
(require 'json)
(require 'thingatpt)

(defvar my-denote-jira-base-url
  (or (getenv "JIRA_BASE_URL") "https://nexthopai.atlassian.net")
  "Base URL of the Jira Cloud instance, used to build browse links.
Defaults to the JIRA_BASE_URL environment variable, falling back to
the Nexthop instance.  The fallback matters when Emacs runs as a
daemon launched outside a login shell (e.g. driven from Hammerspoon),
where the environment variable is absent.")

(defvar my-denote-jira-status-keyword "taskinbox"
  "Status keyword applied to a freshly created Jira task note.")

(defvar my-denote-jira-key-regexp "[A-Z][A-Z0-9]+-[0-9]+"
  "Regexp matching a Jira issue key such as NOS-10358 or DEVX-1667.")

(defvar my-denote-jira-default-project "NOS"
  "Project key prepended when only a bare issue number is given.
So typing \"9140\" yields \"NOS-9140\"; full keys like
\"DEVX-1667\" are used as-is.")

(defun my-denote-jira--normalize-key (input)
  "Return a canonical Jira key from INPUT.
Trim and upcase; if INPUT is a bare number, prepend
`my-denote-jira-default-project'."
  (let ((s (upcase (string-trim input))))
    (if (string-match-p "\\`[0-9]+\\'" s)
        (format "%s-%s" my-denote-jira-default-project s)
      s)))

(defun my-denote-jira-key-at-point ()
  "Return a Jira issue key at or around point, or nil.
Checks the active region first, then a URL under point, then the
symbol at point.  Useful as a default when prompting."
  (let ((candidate
         (cond
          ((use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))
          ((thing-at-point 'url t))
          ((thing-at-point 'symbol t)))))
    (when (and candidate (string-match my-denote-jira-key-regexp candidate))
      (upcase (match-string 0 candidate)))))

(defun my-denote-jira--fetch (key fields)
  "Return the `fields' alist for Jira issue KEY via acli.
FIELDS is a comma-separated field list passed to acli.  JSON
arrays are decoded as lists and objects as alists with symbol
keys.  Signal a `user-error' if the lookup or parse fails."
  (with-temp-buffer
    (let ((status (call-process "acli" nil t nil
                                "jira" "workitem" "view" key
                                "--json" "--fields" fields)))
      (unless (zerop status)
        (user-error "acli failed for %s: %s" key
                    (string-trim (buffer-string))))
      (goto-char (point-min))
      (alist-get 'fields (json-parse-buffer :object-type 'alist
                                            :array-type 'list
                                            :null-object nil)))))

;;; ADF (Atlassian Document Format) -> Org conversion

(defun my-denote-jira--adf-marks (text marks)
  "Apply ADF MARKS to TEXT, returning an Org-formatted string.
MARKS is a list of alists with a `type' and optional `attrs'."
  (let ((s text) (href nil))
    (dolist (m marks)
      (pcase (alist-get 'type m)
        ("code" (setq s (concat "~" s "~")))
        ("strong" (setq s (concat "*" s "*")))
        ("em" (setq s (concat "/" s "/")))
        ("underline" (setq s (concat "_" s "_")))
        ("link" (setq href (alist-get 'href (alist-get 'attrs m))))))
    (if href (format "[[%s][%s]]" href s) s)))

(defun my-denote-jira--adf-inline (nodes)
  "Render inline ADF NODES (a list) to an Org string."
  (mapconcat
   (lambda (n)
     (pcase (alist-get 'type n)
       ("text" (my-denote-jira--adf-marks (alist-get 'text n)
                                          (alist-get 'marks n)))
       ("hardBreak" "\n")
       ((or "mention" "emoji" "inlineCard")
        (let ((a (alist-get 'attrs n)))
          (or (alist-get 'text a) (alist-get 'url a) "")))
       (_ (my-denote-jira--adf-inline (alist-get 'content n)))))
   nodes ""))

(defun my-denote-jira--adf-list (items indent ordered)
  "Render ADF list ITEMS (listItem nodes) to an Org string.
INDENT is a leading-whitespace prefix; ORDERED non-nil numbers items."
  (let ((i 0))
    (mapconcat
     (lambda (item)
       (setq i (1+ i))
       (let* ((bullet (if ordered (format "%d. " i) "- "))
              (child-indent (concat indent (make-string (length bullet) ?\s)))
              (blocks (alist-get 'content item))
              (lead (car blocks))
              (lead-para (equal (alist-get 'type lead) "paragraph")))
         (concat indent bullet
                 (if lead-para (my-denote-jira--adf-inline (alist-get 'content lead)) "")
                 "\n"
                 (my-denote-jira--adf-blocks (if lead-para (cdr blocks) blocks)
                                             child-indent))))
     items "")))

(defun my-denote-jira--adf-blocks (nodes &optional indent)
  "Render block-level ADF NODES (a list) to an Org string.
INDENT is a leading-whitespace prefix for nested content."
  (let ((indent (or indent "")))
    (mapconcat
     (lambda (n)
       (pcase (alist-get 'type n)
         ("paragraph"
          (concat indent (my-denote-jira--adf-inline (alist-get 'content n)) "\n"))
         ("heading"
          (let ((level (or (alist-get 'level (alist-get 'attrs n)) 1)))
            (concat (make-string (1+ level) ?*) " "
                    (my-denote-jira--adf-inline (alist-get 'content n)) "\n")))
         ("codeBlock"
          (let ((lang (or (alist-get 'language (alist-get 'attrs n)) ""))
                (code (my-denote-jira--adf-inline (alist-get 'content n))))
            (if (string-empty-p lang)
                (format "#+begin_example\n%s\n#+end_example\n" code)
              (format "#+begin_src %s\n%s\n#+end_src\n" lang code))))
         ("bulletList" (my-denote-jira--adf-list (alist-get 'content n) indent nil))
         ("orderedList" (my-denote-jira--adf-list (alist-get 'content n) indent t))
         ("blockquote"
          (concat indent "#+begin_quote\n"
                  (my-denote-jira--adf-blocks (alist-get 'content n) indent)
                  indent "#+end_quote\n"))
         ("rule" (concat indent "-----\n"))
         (_ (my-denote-jira--adf-blocks (alist-get 'content n) indent))))
     nodes "")))

(defun my-denote-jira--adf-to-org (doc)
  "Convert ADF DOC alist to a trimmed Org-formatted string."
  (string-trim (my-denote-jira--adf-blocks (alist-get 'content doc))))

(defun my-denote-jira-new-task (key keywords &optional with-description)
  "Create a Denote task note for Jira issue KEY.
KEYWORDS are the topic keyword(s); `my-denote-jira-status-keyword'
is appended automatically.  The note title is \"KEY: Summary\",
where the summary is fetched from Jira.

With a prefix argument (WITH-DESCRIPTION), also fetch the Jira
description, convert it from ADF to Org, and insert it under a
`* Description' heading.

Interactively, prompt for KEY (defaulting to a key at point) and
for topic keywords with completion against known Denote keywords."
  (interactive
   (let* ((default-key (my-denote-jira-key-at-point))
          (key (read-string (format-prompt "Jira issue key" default-key)
                            nil nil default-key)))
     (list (my-denote-jira--normalize-key key)
           (denote-keywords-prompt "Topic keyword(s)")
           current-prefix-arg)))
  (let* ((fields (my-denote-jira--fetch
                  key (if with-description "summary,description" "summary")))
         (summary (alist-get 'summary fields)))
    (unless (and summary (stringp summary))
      (user-error "No summary found for %s" key))
    (let* ((title (format "%s: %s" key summary))
           (all-keywords (append keywords (list my-denote-jira-status-keyword)))
           (description (when-let* ((adf (and with-description
                                              (alist-get 'description fields)))
                                    (org (my-denote-jira--adf-to-org adf)))
                          (unless (string-empty-p org) org))))
      (denote title all-keywords)
      ;; `denote' leaves us in the new note, just past the front matter.
      (goto-char (point-max))
      (insert (format "\n[[%s/browse/%s][JIRA: %s]]\n\n"
                      my-denote-jira-base-url key key)
              "# PR: \n\n"
              "# Attachment: \n")
      (when description
        (insert "\n* Description\n\n" description "\n")))))

(global-set-key (kbd "C-c n t") #'my-denote-jira-new-task)


;;; Status lifecycle

(defvar my-denote-jira-status-keywords
  '("taskinbox" "taskinprogress" "doreview" "docodereview" "review" "taskdone")
  "All status keywords used in the task lifecycle.
Any keyword in this list is treated as a status (and is replaced
when bumping status); the list also seeds completion in
`my-denote-jira-bump-status'.  Add new states here as the
workflow grows.")

(defun my-denote-jira--current-file ()
  "Return the Denote file for the current buffer, or signal an error."
  (or (when-let* ((file (buffer-file-name)))
        (and (denote-file-has-denoted-filename-p file) file))
      (user-error "Current buffer is not visiting a Denote note")))

(defun my-denote-jira-bump-status (status)
  "Set the lifecycle STATUS keyword of the current Denote note.
Remove any existing status keywords (those in
`my-denote-jira-status-keywords') and add STATUS, leaving topic
keywords untouched.  Renames the file and rewrites the front
matter in one go.

Interactively, prompt for STATUS with completion against
`my-denote-jira-status-keywords'."
  (interactive
   (list (completing-read "New status: " my-denote-jira-status-keywords
                          nil :require-match)))
  (let* ((file (my-denote-jira--current-file))
         (file-type (denote-filetype-heuristics file))
         (date (denote-retrieve-front-matter-date-value file file-type))
         (identifier (or (denote-retrieve-filename-identifier file) ""))
         (title (or (denote-retrieve-title-or-filename file file-type) ""))
         (signature (or (denote-retrieve-filename-signature file) ""))
         (keywords (denote-extract-keywords-from-path file))
         (topics (seq-remove (lambda (kw)
                               (member kw my-denote-jira-status-keywords))
                             keywords))
         (new-keywords (append topics (list status)))
         (denote-rename-confirmations nil))
    (denote-rename-file file title new-keywords signature date identifier)
    (message "Status -> %s" status)))

(global-set-key (kbd "C-c n S") #'my-denote-jira-bump-status)


;;; Link helpers

(defun my-denote-jira--clipboard ()
  "Return the OS clipboard text as a trimmed string, or nil if empty."
  (when-let* ((raw (or (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING))
                       (ignore-errors (current-kill 0 t))))
              (s (string-trim (substring-no-properties raw))))
    (unless (string-empty-p s) s)))

(defun my-denote-jira--url-p (s)
  "Return non-nil if S looks like an http(s) URL."
  (and (stringp s) (string-match-p "\\`https?://" s)))

(defun my-denote-jira--external-uri-p (s)
  "Return non-nil if S looks like an external URI (has a scheme)."
  (and (stringp s) (string-match-p "://" s)))

(defun my-denote-jira--pr-description (url)
  "Derive a link description from a GitHub PR URL, else \"pull request\"."
  (if (string-match "github\\.com/\\([^/]+\\)/\\([^/]+\\)/pull/\\([0-9]+\\)" url)
      (format "%s/%s#%s"
              (match-string 1 url) (match-string 2 url) (match-string 3 url))
    "pull request"))

(defun my-denote-jira--goto-body-top ()
  "Move point to the start of the note body, just after the front matter."
  (goto-char (point-min))
  (if (re-search-forward "^#\\+identifier:.*\n" nil t)
      (when (looking-at-p "[ \t]*\n") (forward-line 1))
    (goto-char (point-min))))

(defun my-denote-jira-insert-pr (url description)
  "Insert an Org link to PR URL near the top of the current note.
If a \"# PR:\" placeholder line exists, replace it in place;
otherwise insert just after the front matter.

Interactively, default URL to the clipboard (when it looks like a
URL) and default DESCRIPTION to a value derived from a GitHub PR
URL."
  (interactive
   (let* ((clip (my-denote-jira--clipboard))
          (default-url (and (my-denote-jira--url-p clip) clip))
          (url (string-trim
                (read-string (format-prompt "PR URL" default-url)
                             nil nil default-url))))
     (list url (read-string "Description: " (my-denote-jira--pr-description url)))))
  (when (string-empty-p url) (user-error "No PR URL given"))
  (my-denote-jira--current-file)
  (let ((link (format "[[%s][%s]]" url description)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#[ \t]*PR:.*$" nil t)
          (replace-match link t t)
        (my-denote-jira--goto-body-top)
        (insert link "\n\n")))
    (message "Inserted PR link")))

(global-set-key (kbd "C-c n p") #'my-denote-jira-insert-pr)

(defun my-denote-jira--attachments-dir ()
  "Return the absolute path of the attachments/ directory."
  (expand-file-name "attachments" denote-directory))

(defun my-denote-jira-insert-attachment (target description)
  "Insert an Org link to an attachment at point.
TARGET is either a file name within the attachments/ directory or
an external URI.  If an unused \"# Attachment:\" placeholder line
exists, replace it; otherwise insert the link at point.

Interactively, complete against files in attachments/ while also
accepting free-form input (e.g. a URL)."
  (interactive
   (let* ((dir (my-denote-jira--attachments-dir))
          (files (when (file-directory-p dir)
                   (directory-files dir nil "\\`[^.]")))
          (target (completing-read
                   "Attachment (file in attachments/ or URI): " files))
          (default-desc (if (my-denote-jira--external-uri-p target)
                            target
                          (file-name-base target))))
     (list target (read-string "Description: " default-desc))))
  (my-denote-jira--current-file)
  (let* ((path (if (my-denote-jira--external-uri-p target)
                   target
                 (format "file:attachments/%s" target)))
         (link (format "[[%s][%s]]" path description)))
    (if (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^#[ \t]*Attachment:.*$" nil t)
            (replace-match link t t)
            t))
        (message "Filled attachment slot")
      (insert link)
      (message "Inserted attachment link"))))

(global-set-key (kbd "C-c n a") #'my-denote-jira-insert-attachment)


;;; Create from assigned issues

(defvar my-denote-jira-assigned-jql
  "assignee = currentUser() AND statusCategory != Done ORDER BY updated DESC"
  "JQL listing candidate issues for `my-denote-jira-new-task-from-assigned'.")

(defun my-denote-jira--search (jql)
  "Run a JQL search via acli and return a list of issue alists."
  (with-temp-buffer
    (let ((status (call-process "acli" nil t nil
                                "jira" "workitem" "search"
                                "--jql" jql
                                "--fields" "key,summary,status"
                                "--limit" "100" "--json")))
      (unless (zerop status)
        (user-error "acli search failed: %s" (string-trim (buffer-string))))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil))))

(defun my-denote-jira-new-task-from-assigned (key keywords &optional with-description)
  "Pick one of my open Jira issues and create a Denote task note for it.
Like `my-denote-jira-new-task', but obtains KEY by completion over
issues returned by `my-denote-jira-assigned-jql' rather than
prompting for a key.  A prefix argument inserts the description."
  (interactive
   (let ((issues (my-denote-jira--search my-denote-jira-assigned-jql)))
     (unless issues (user-error "No assigned issues found"))
     (let* ((candidates
             (mapcar
              (lambda (it)
                (let* ((fields (alist-get 'fields it))
                       (summary (or (alist-get 'summary fields) ""))
                       (status (or (alist-get 'name (alist-get 'status fields)) "")))
                  (cons (format "%-11s [%s] %s" (alist-get 'key it) status summary)
                        (alist-get 'key it))))
              issues))
            (choice (completing-read "Issue: " candidates nil :require-match)))
       (list (cdr (assoc choice candidates))
             (denote-keywords-prompt "Topic keyword(s)")
             current-prefix-arg))))
  (my-denote-jira-new-task key keywords with-description))

(global-set-key (kbd "C-c n T") #'my-denote-jira-new-task-from-assigned)


;;; Navigation

(defun my-denote-jira--buffer-key ()
  "Return the Jira key from the current note's title, or nil."
  (let* ((file (my-denote-jira--current-file))
         (title (denote-retrieve-title-or-filename
                 file (denote-filetype-heuristics file))))
    (when (and title (string-match my-denote-jira-key-regexp title))
      (upcase (match-string 0 title)))))

(defun my-denote-jira--buffer-pr-urls ()
  "Return GitHub PR URLs linked in the current buffer, in order."
  (let (urls)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "\\[\\[\\(https?://[^]]*github\\.com/[^]]*/pull/[0-9]+[^]]*\\)\\]" nil t)
        (push (match-string-no-properties 1) urls)))
    (nreverse (delete-dups urls))))

(defun my-denote-jira-open (target)
  "Open the Jira issue or a linked PR for the current note in a browser.
If the issue is the only candidate, open it directly; otherwise
prompt to choose among the issue and any linked GitHub PRs."
  (interactive
   (let* ((key (my-denote-jira--buffer-key))
          (jira (when key
                  (cons (format "JIRA: %s" key)
                        (format "%s/browse/%s" my-denote-jira-base-url key))))
          (prs (mapcar (lambda (u) (cons (format "PR:   %s" u) u))
                       (my-denote-jira--buffer-pr-urls)))
          (candidates (delq nil (cons jira prs))))
     (unless candidates
       (user-error "No Jira key or PR link found in this note"))
     (list (if (= (length candidates) 1)
               (cdar candidates)
             (cdr (assoc (completing-read "Open: " candidates nil :require-match)
                         candidates))))))
  (browse-url target))

(global-set-key (kbd "C-c n o") #'my-denote-jira-open)

(defun my-denote-jira-visit-by-key (key)
  "Visit the Denote note for Jira KEY.
If several notes match, prompt to choose; if none exist, offer to
create one with `my-denote-jira-new-task'.  Intended both for
interactive use and for invocation via emacsclient (e.g. from a
Hammerspoon hotkey while viewing the ticket in a browser)."
  (interactive
   (list (my-denote-jira--normalize-key (read-string "Jira key: "))))
  (let* ((key (my-denote-jira--normalize-key key))
         ;; Match the key at the start of the title slug (the "KEY: Summary"
         ;; convention), so incidental mentions elsewhere in a note's title
         ;; are ignored.  The trailing class bounds it so that e.g. NOS-914
         ;; does not match nos-9140.
         (regexp (format "--%s\\([^0-9]\\|$\\)" (downcase (regexp-quote key))))
         (files (denote-directory-files regexp)))
    (cond
     ((null files)
      (when (yes-or-no-p (format "No note for %s.  Create one? " key))
        (my-denote-jira-new-task key (denote-keywords-prompt "Topic keyword(s)"))))
     ((= (length files) 1)
      (find-file (car files)))
     (t (find-file (completing-read "Choose note: " files nil :require-match))))
    ;; Raise the frame when called from emacsclient.
    (when (display-graphic-p)
      (select-frame-set-input-focus (selected-frame)))))

(global-set-key (kbd "C-c n v") #'my-denote-jira-visit-by-key)

(provide 'denote-jira)
;;; denote-jira.el ends here
