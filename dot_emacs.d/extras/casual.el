(use-package casual ;; A collection of  user interfaces for various built-in Emacs modes.
  :straight t
  :general
  (:keymaps 'org-agenda-mode-map "C-o" 'casual-agenda-tmenu)
  (:keymaps 'bookmark-bmenu-mode-map "C-o" 'casual-bookmarks-tmenu)
  (:keymaps 'calc-mode-map "C-o" 'casual-calc-tmenu)
  (:keymaps 'dired-mode-map "C-o" 'casual-dired-tmenu)
  (:keymaps 'Info-mode-map "C-o" 'casual-info-tmenu))

