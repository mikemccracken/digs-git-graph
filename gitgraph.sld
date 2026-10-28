;; Git history diagrams for Digs, in the style of Mermaid's gitGraph.
;;
;;   (git-graph origin event … #:gap 50 #:lane-gap 50 #:direction 'right|'down
;;              #:main "main" #:colors (list color …))
;;
;; Events are read in order, starting on the main branch, and every commit
;; takes the next column, on its branch's lane. The first commit is at origin,
;; a point. Events are plain lists, so they can be quoted,
;; '(commit "init" #:tag "v1"), or built with map; lists of events are flattened.
(define-library (gitgraph)
  (export git-graph commit branch checkout merge)
  (import (digs))
  (begin
    ;; --- Events ------------------------------------------------------------

    (define (commit . args) (cons 'commit args))
    (define (branch name . options) (cons 'branch (cons name options)))
    (define (checkout name) (list 'checkout name))
    (define (merge name . args) (cons 'merge (cons name args)))

    ;; --- Arguments ---------------------------------------------------------

    ;; Arguments that aren't #:keyword value options.
    (define (positional args)
      (cond ((null? args) '())
            ((keyword? (car args)) (if (pair? (cdr args)) (positional (cddr args)) '()))
            (else (cons (car args) (positional (cdr args))))))

    (define (option args key default)
      (cond ((null? args) default)
            ((and (eq? (car args) key) (pair? (cdr args))) (cadr args))
            (else (option (cdr args) key default))))

    (define (one-of value choices)
      (if (memq value choices)
          value
          (error "git-graph: expected one of" choices 'but 'got value)))

    ;; Events from git-graph's arguments: skips options, flattens lists.
    (define (events-in args)
      (cond ((null? args) '())
            ((keyword? (car args)) (if (pair? (cdr args)) (events-in (cddr args)) '()))
            ((and (pair? (car args)) (symbol? (caar args))) (cons (car args) (events-in (cdr args))))
            ((or (pair? (car args)) (null? (car args))) (append (events-in (car args)) (events-in (cdr args))))
            (else (error "git-graph: expected events such as (commit \"init\"), got" (car args)))))

    (define (name-in e)
      (if (pair? (cdr e)) (cadr e) (error "git-graph: expected a branch name in" e)))

    ;; Branches are looked up by symbol; a string name works too.
    (define (key-of name)
      (cond ((symbol? name) name)
            ((string? name) (string->symbol name))
            (else (error "git-graph: a branch name must be a symbol or a string, got" name))))

    ;; --- Branches ----------------------------------------------------------

    ;; A branch: (key lane head name color). Its head is the last commit on
    ;; it, (column lane), or #f before its first. `name` is drawn as written.
    (define (make-branch key lane head name color) (list key lane head name color))
    (define (lane-of b) (cadr b))
    (define (head-of b) (caddr b))
    (define (name-of b) (list-ref b 3))
    (define (color-of b) (list-ref b 4))
    (define (move-head b head) (make-branch (car b) (lane-of b) head (name-of b) (color-of b)))
    (define (replace branches b) (map (lambda (e) (if (eq? (car e) (car b)) b e)) branches))

    ;; The branch to merge into `cur`, with a head to merge.
    (define (merge-source e cur branches)
      (let* ((key (key-of (name-in e)))
             (src (assq key branches)))
        (cond ((not src) (error "git-graph: no branch named" key))
              ((eq? key cur) (error "git-graph: can't merge a branch into itself:" key))
              ((or (not (head-of src)) (eq? (head-of src) (head-of (assq cur branches))))
               (error "git-graph: nothing to merge from" key))
              (else src))))

    ;; --- Drawing -----------------------------------------------------------

    (define palette '("#1c7ed6" "#2b8a3e" "#e8590c" "#ae3ec9" "#0c8599" "#f08c00" "#c2255c"))

    ;; Bounds, as (bounds shape) returns them: (x y width height).
    (define (left b) (car b))
    (define (top b) (cadr b))
    (define (width b) (caddr b))
    (define (height b) (list-ref b 3))
    (define (right b) (+ (left b) (width b)))

    ;; A line from commit `from` to commit `to`. Between lanes it bends within
    ;; one column: next to `from` when a branch leaves (early), next to `to`
    ;; when a merge arrives, so it runs along the lane of the branch it
    ;; belongs to.
    (define (edge at from to color early)
      (let ((fc (car from)) (fl (cadr from)) (tc (car to)) (tl (cadr to)))
        (if (= fl tl)
            (path (move-to (at fc fl)) (line-to (at tc tl)) #:stroke color #:stroke-width 3)
            (let ((c (if early fc (- tc 1))))
              (path (move-to (at fc fl))
                    (if (> c fc) (line-to (at c fl)) '())
                    (curve-to (at (+ c 0.5) fl) (at (+ c 0.5) tl) (at (+ c 1) tl))
                    (if (< (+ c 1) tc) (line-to (at tc tl)) '())
                    #:stroke color #:stroke-width 3)))))

    (define (node p color type merge?)
      (let ((x (car p)) (y (cadr p)))
        (cond ((eq? type 'highlight) (rect (pt (- x 7) (- y 7)) 14 14 #:fill color #:stroke "white" #:stroke-width 2))
              ((eq? type 'reverse)
               (group (circle (pt x y) 8 #:fill color #:stroke "white" #:stroke-width 2)
                      (line (pt (- x 3) (- y 3)) (pt (+ x 3) (+ y 3)) #:stroke "white" #:stroke-width 2)
                      (line (pt (- x 3) (+ y 3)) (pt (+ x 3) (- y 3)) #:stroke "white" #:stroke-width 2)))
              (merge? (circle (pt x y) 6 #:fill "white" #:stroke color #:stroke-width 4))
              (else (circle (pt x y) 8 #:fill color #:stroke "white" #:stroke-width 2)))))

    ;; Text on a rounded box, its middle at height y.
    (define (pill x y content anchor size fill ink)
      (let* ((t (text (pt x (+ y (* size 0.3))) content
                      #:anchor anchor #:font-size size #:font-weight "bold" #:fill ink))
             (b (bounds t)))
        (list (rect (pt (- (left b) 5) (- (top b) 3)) (+ (width b) 10) (+ (height b) 6) #:rx 4 #:fill fill) t)))

    (define (tag-pill x y tag anchor) (pill x y tag anchor 10 "#fff3bf" "#664d03"))

    ;; A commit label, with a white outline so it stays readable over lines.
    (define (label-text x y label anchor)
      (text (pt x y) label #:anchor anchor #:font-size 11 #:fill "#495057"
            #:stroke "white" #:stroke-width 3 #:paint-order "stroke"))

    ;; A commit's label and tag: below and above it when the graph runs
    ;; right; in a column to the right of every lane, tag after label, when it
    ;; runs down.
    (define (labels at dir lanes c label tag)
      (let ((p (at (car c) (cadr c))))
        (if (eq? dir 'right)
            (list (if label (label-text (car p) (+ (cadr p) 24) label "middle") '())
                  (if tag (tag-pill (car p) (- (cadr p) 20) tag "middle") '()))
            (let* ((x (+ (car (at 0 (- lanes 1))) 24))
                   (t (and label (label-text x (+ (cadr p) 4) label "start"))))
              (list (or t '())
                    (if tag (tag-pill (if t (+ (right (bounds t)) 12) (+ x 5)) (cadr p) tag "start") '()))))))

    ;; Each branch's lane, from the first column to the last, and its name
    ;; before it.
    (define (lane at dir columns b)
      (let ((start (at 0 (lane-of b))))
        (list (if (> columns 1)
                  (line start (at (- columns 1) (lane-of b))
                        #:stroke (color-of b) #:stroke-width 1 #:dash "2 5" #:opacity 0.5)
                  '())
              (if (eq? dir 'right)
                  (pill (- (car start) 20) (cadr start) (name-of b) "end" 12 (color-of b) "white")
                  ;; Above the lane, every other one higher so neighbouring
                  ;; names don't overlap.
                  (pill (car start) (- (cadr start) (if (odd? (lane-of b)) 46 24))
                        (name-of b) "middle" 12 (color-of b) "white")))))

    ;; --- The graph ---------------------------------------------------------

    (define (git-graph origin . args)
      (let* ((x (x-of origin))
             (y (y-of origin))
             (gap (option args #:gap 50))
             (lane-gap (option args #:lane-gap 50))
             (dir (one-of (option args #:direction 'right) '(right down)))
             (colors (option args #:colors palette))
             (main (option args #:main "main"))
             ;; Column and lane (either can be fractional) to a point (x y).
             (at (lambda (col lane)
                   (if (eq? dir 'right)
                       (list (+ x (* col gap)) (+ y (* lane lane-gap)))
                       (list (+ x (* lane lane-gap)) (+ y (* col gap)))))))
        (let loop ((events (events-in args))
                   (col 0)
                   (cur (key-of main))
                   (branches (list (make-branch (key-of main) 0 #f main (car colors))))
                   (edges '())
                   ;; Each commit: ((column lane) color type merge? label tag)
                   (commits '()))
          (if (null? events)
              (let ((commits (reverse commits)))
                (group (map (lambda (b) (lane at dir col b)) branches)
                       (reverse edges)
                       (map (lambda (c) (node (at (caar c) (cadr (car c))) (cadr c) (caddr c) (list-ref c 3)))
                            commits)
                       (map (lambda (c) (labels at dir (length branches) (car c) (list-ref c 4) (list-ref c 5)))
                            commits)))
              (let* ((e (car events))
                     (kind (car e))
                     (b (assq cur branches)))
                (cond ((memq kind '(commit merge))
                       (let* ((src (and (eq? kind 'merge) (merge-source e cur branches)))
                              (options (if src (cddr e) (cdr e)))
                              (label (let ((pos (positional options))) (if (pair? pos) (car pos) #f)))
                              (type (one-of (option options #:type 'normal) '(normal highlight reverse)))
                              (c (list col (lane-of b)))
                              (head (head-of b)))
                         (loop (cdr events)
                               (+ col 1)
                               cur
                               (replace branches (move-head b c))
                               (append (if head (list (edge at head c (color-of b) #t)) '())
                                       (if src (list (edge at (head-of src) c (color-of src) #f)) '())
                                       edges)
                               (cons (list c (color-of b) type (and src #t) label (option options #:tag #f))
                                     commits))))
                      ((eq? kind 'branch)
                       (let* ((name (name-in e))
                              (key (key-of name))
                              (lane (length branches))
                              (color (option (cddr e) #:color (list-ref colors (modulo lane (length colors))))))
                         (when (assq key branches) (error "git-graph: there is already a branch named" key))
                         (loop (cdr events) col key
                               (append branches (list (make-branch key lane (head-of b) name color)))
                               edges commits)))
                      ((eq? kind 'checkout)
                       (let ((key (key-of (name-in e))))
                         (unless (assq key branches) (error "git-graph: no branch named" key))
                         (loop (cdr events) col key branches edges commits)))
                      (else (error "git-graph: expected commit, branch, checkout or merge, got" e))))))))

    ;; A toolbar tool: drag out a small graph to start from. The sample is
    ;; copied into the program as written, so it's kept on one line.
    (tool (git-graph (pt 0 0) (commit "init") (branch 'feature) (commit "work") (checkout 'main) (merge 'feature))
          #:name "Git graph" #:key "h")))
