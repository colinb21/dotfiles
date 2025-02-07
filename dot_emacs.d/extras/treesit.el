(setq treesit-language-source-alist
      '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (scheme "https://github.com/6cdh/tree-sitter-scheme")
        (c  "https://github.com/tree-sitter/tree-sitter-c")
        (c++ "https://github.com/tree-sitter/tree-sitter-cpp")))



;; maybe only needed for emacs29. See:
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))
