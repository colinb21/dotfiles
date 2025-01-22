
(use-package  rustic
  :bind (:map rustic-mode-map

              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(use-package  dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
	 :miDebuggerPath "lldb-mi"
         :target nil
         :cwd nil))
  )

(with-eval-after-load 'lsp-rust
    (require 'dap-cpptools))

(setq lsp-rust-analyzer-debug-lens-extra-dap-args
      `(:MIMode "lldb"
		:miDebuggerPath "/usr/local/bin/lldb-mi"
		:stopAtEntry t
		:externalConsole
		:json-false))

;; Quick start (https://github.com/grafov/rust-playground)
;;
;;    From any mode run M-x rust-playground for start a new playground
;;    buffer filled with basic template for the package with main
;;    function (see the picture below).
;;
;;    Add your code then press Ctl-Return (it bound to
;;    rust-playground-exec command). It will save, compile and exec
;;    the snippet code.
;;
;;    When you played enough with this snippet just run M-x
;;    rust-playground-rm. It will remove the current snippet with its
;;    directory and all files.
(use-package rust-playground)

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
