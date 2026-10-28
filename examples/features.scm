(import (gitgraph))

;; Events are lists, so a procedure can return several.
(define (feature name . messages)
  (list (branch name)
        (map commit messages)
        (checkout 'main)
        (merge name)))

(diagram 300 300
  (git-graph (pt 50 70)
    (commit "start")
    (feature 'search "index" "query")
    (feature 'export "csv")
    (commit "release" #:tag "v2")
    #:direction 'down #:gap 36))
