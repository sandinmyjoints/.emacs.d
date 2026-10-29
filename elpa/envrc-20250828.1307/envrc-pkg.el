;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "envrc" "20250828.1307"
  "Support for `direnv' that operates buffer-locally."
  '((emacs      "27.1")
    (inheritenv "0.1")
    (seq        "2.24"))
  :url "https://github.com/purcell/envrc"
  :commit "48a2ee6a4d6a36b1dc76efa321b9b9c8bc11d73b"
  :revdesc "48a2ee6a4d6a"
  :keywords '("processes" "tools")
  :authors '(("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")))
