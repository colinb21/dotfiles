(use-package  lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode
	  c++-mode
	  c-or-c++-mode
;;	  java-mode
      python-mode-hook) . lsp-deferred)
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'java-mode-hook #'(lambda() (when (eq major-mode 'java-mode) (lsp-deferred))))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))

(use-package  lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
