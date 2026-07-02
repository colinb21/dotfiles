if [[ "$INSIDE_EMACS" = 'ghostel' ]]; then
    # Open a file in Emacs from the terminal
    e()   { ghostel_cmd find-file-other-window "$@"; }

    # Open dired in another window
    dow() { ghostel_cmd dired-other-window "$@"; }

    # Open magit for the current directory
    gst() { ghostel_cmd magit-status-setup-buffer "$(pwd)"; }
fi
