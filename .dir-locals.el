((clojure-mode
  (clojure-indent-style . 'always-align)
  (clojure-special-arg-indent-factor . 1) ; for cljfmt equivalence
  (cider-preferred-build-tool . "clojure-cli")
  (cider-clojure-cli-aliases . "dev:test")
  (eval .
        (define-clojure-indent
          ;; Please keep this list sorted
          (arg-gen/measured 2)))
  (fill-column . 80)
  (comment-fill-column . 72)))
