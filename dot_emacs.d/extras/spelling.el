;; Jinx: Enchanted spell-checking
(use-package jinx
  :hook (((text-mode prog-mode) . jinx-mode))
  :bind (("C-;" . jinx-correct))
  :config (push "[\U00002600-\U0001ffff]"
                (alist-get t jinx-exclude-regexps))
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.01)
  (jinx-languages "en"))

(setopt dictionary-use-single-buffer t)
(setopt dictionary-server "dict.org")

;; Interface for sdcv (StartDict console version)..
;;
;; largely stolen from
;; https://github.com/jamescherti/quick-sdcv.el/discussions/6
(use-package quick-sdcv
  :bind
  ("C-c s" . my-quick-sdcv-dwim)
  (:map quick-sdcv-mode-map
		("q" . quit-window)
		("<tab>" . outline-toggle-children))
  :hook
  (quick-sdcv-mode . goto-address-mode)
  :config
  (add-to-list 'display-buffer-alist
			   '("\\*sdcv"
				 (display-buffer-reuse-window display-buffer-at-bottom)
				 (window-height . 0.3)))
  :custom
  (quick-sdcv-dictionary-prefix-symbol "▶")
  (quick-sdcv-ellipsis " ▼")
  (quick-sdcv-unique-buffers t))


(defun my-quick-sdcv-dwim (&optional arg)
  "Look up a word using quick-sdcv.
By default, looks up the word at point.
With a prefix argument (\\[universal-argument]), prompts for a word."
  (interactive "P")
  (if arg
	  (call-interactively #'quick-sdcv-search-input)
	(quick-sdcv-search-at-point)))

;; Cape-jinx package and configuration:
;; sounds wonderful but keep crashing with some sort of cache error in
;; emacs 29. Maybe try again with a later version
;; (use-package cape-jinx-completion
;;   :straight (:type git
;;                    :repo "https://code.bsdgeek.org/adam/cape-jinx-completion"
;;                    :files (:defaults "*.el"))
;;   :after jinx
;;   :config
;;   ;; add cape-jinx to completion-at-point functions list.
;;   (add-to-list 'completion-at-point-functions #'cape-jinx-completion))
