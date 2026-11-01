;; macOS 27's libgccjit miscomputes -mmacosx-version-min as (darwin-major - 9),
;; which clang now rejects, breaking native compilation of advice trampolines.
;; Disable subr trampolines until the bundled Emacs build ships a fixed libgccjit.
(setq native-comp-enable-subr-trampolines nil)
