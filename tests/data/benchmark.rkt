#lang racket/base

(require benchmark
         plot
         racket/class
         racket/match
         data/hamt
         (prefix-in f: data/hamt/fast)
         racket/syntax
         (for-syntax racket/base))

(define (random-key)
  (list->string
   (map integer->char
        (for/list ([i (in-range 1 (add1 (random 20)))])
          (random 256)))))


(define N 50000)


(define (gc)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage))

(define-syntax-rule (run op keys kons set ref rem)
  (begin
    (gc)
    (match op
     ['insert
      (time (for/fold ([h (kons)]) ([k (in-list keys)]) (set h k #t)))]
     ['lookup
      (let ([h (for/fold ([h (kons)]) ([k (in-list keys)]) (set h k #t))])
        (gc)
        (time (for ([k (in-list keys)]) (ref h k))))]
     ['remove
      (let ([h (for/fold ([h (kons)]) ([k (in-list keys)]) (set h k #t))])
        (gc)
        (void (time (for/fold ([h h]) ([k (in-list keys)]) (rem h k)))))])))

(define (get-results kind)
  (run-benchmarks
   (list 'insert 'lookup 'remove)
   (list (list 'hash 'hamt 'f:hamt)
         (list kind))
   (lambda (op map-kind test-kind)
     (define-values (keys mk-hash mk-hamt mk-f:hamt)
       (match test-kind
        ['rand-str
         (values (for/list ([i N]) (random-key)) hash hamt f:hamt)]
        ['seq-int
         (values (for/list ([i (in-range N)]) i) hasheqv hamteqv f:hamteqv)]
        ['rand-int
         (values (for/list ([i (in-range N)]) (random 1000000000))
                 hasheqv hamteqv f:hamteqv)]
        ['rand-sym
         (values (for/list ([i (in-range N)]) (string->symbol (random-key)))
                 hasheq hamteq f:hamteq)]))
     (match map-kind
       ['hash
        (run op keys mk-hash hash-set hash-ref hash-remove)]
       ['hamt
        (run op keys mk-hamt hamt-set hamt-ref hamt-remove)]
       ['f:hamt
        (run op keys mk-f:hamt f:hamt-set f:hamt-ref f:hamt-remove)]))
   #:num-trials 10
   #:results-file "benchmark-results"))

(define (do-plot results kind filename)
  (parameterize ([plot-x-ticks no-ticks])
    (plot-file
     (render-benchmark-alts
      (list 'hash kind)
      results)
     #:title "hash vs. hamt vs. fast hamt"
     #:x-label #f
     #:y-label "normalized time"
     filename)))

(for ([kind (list 'rand-str 'seq-int 'rand-int 'rand-sym)])
  (define results (get-results kind))
  (do-plot results kind (string-append (symbol->string kind) ".png")))
