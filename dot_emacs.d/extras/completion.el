(use-package  vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

(use-package  vertico-prescient)
;; After installing the package
(vertico-prescient-mode 1)

;; Configure `prescient.el' filtering to your liking.
(setq prescient-filter-method '(literal initialism prefix regexp)
      prescient-use-char-folding t
      prescient-use-case-folding 'smart
      prescient-sort-full-matches-first t ; Works well with `initialism'.
      prescient-sort-length-enable t)

;; Save recency and frequency rankings to disk, which let them become better
;; over time.
(prescient-persist-mode 1)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode)
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-auto-delay 0)
  ;; (corfu-auto-prefix 1)
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width)     ; Always have the same width
  ;; (corfu-count 14)
  (corfu-scroll-margin 4)

  :bind
  (("C-<tab>" . complete-symbol))
  :config
;;  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer))

;; Add extensions
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         :init
         ;; Add to the global default value of `completion-at-point-functions' which is
         ;; used by `completion-at-point'.  The order of the functions matters, the
         ;; first function returning a result wins.  Note that the list of buffer-local
         ;; completion functions takes precedence over the global list.
         (add-hook 'completion-at-point-functions #'cape-dabbrev)
         (add-hook 'completion-at-point-functions #'cape-file)
         (add-hook 'completion-at-point-functions #'cape-elisp-block)
         )
  

;; built-in to emacs 30. Consider using them when upgrading.
;; see also https://jtamagnan.com/posts/%C3%A0-la-mode-corfu-cape-and-completion-preview/
;; (use-package completion-preview)
;; (global-completion-preview-mode 1)
