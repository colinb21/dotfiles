;; Jinx: Enchanted spell-checking
(use-package jinx
  :hook (((text-mode prog-mode) . jinx-mode))
  :bind (("C-;" . jinx-correct))
  :config (push "[\U00002600-\U0001ffff]"
                (alist-get t jinx-exclude-regexps))
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.01))

(setopt dictionary-use-single-buffer t)
(setopt dictionary-server "dict.org")

;; Interface for sdcv (StartDict console version).
(use-package quick-sdcv
  :ensure t
  :straight (quick-sdcv
             :type git
             :host github
             :repo "jamescherti/quick-sdcv.el")
  :custom
  (quick-sdcv-dictionary-prefix-symbol "►")
  (quick-sdcv-ellipsis " ▼ "))
(global-set-key (kbd "C-c s") 'quick-sdcv-search-at-point)

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
