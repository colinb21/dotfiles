;; mermaid-mode gives syntax highlighting in mermaid files.
(use-package mermaid-mode :hook (mermaid-mode . mermaid-docker-mode))

;; mermaid-docker-mode makes the bloody things renderable. It is slow
;; the first time it runs because it needs to download the relevant
;; docker image. Subsequent runs are much faster. In all cases it uses
;; Preview to view the resulting png file.

;; this will fail (silently) if [Docker|Podman] desktop isn't running.
(use-package mermaid-docker-mode
  :custom
  (mermaid-docker-external t)
  (mermaid-docker-external-viewer-bin "open")
  (mermaid-docker-bin (let ((docker-path "/usr/local/bin/docker")
                            (podman-path "/opt/podman/bin/podman"))
                        (cond ((file-exists-p docker-path) "docker")
                              ((file-exists-p podman-path) "podman")))))


