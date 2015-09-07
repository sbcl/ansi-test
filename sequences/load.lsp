;;; Tests of sequences
(compile-and-load "ANSI-TESTS:AUX;search-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;subseq-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;remove-aux.lsp")
(compile-and-load "ANSI-TESTS:AUX;remove-duplicates-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-pathname*))))
  (load "copy-seq.lsp")
  (load "elt.lsp")
  (load "fill.lsp")
  (load "fill-strings.lsp")
  (load "make-sequence.lsp")
  (load "map.lsp")
  (load "map-into.lsp")
  (load "reduce.lsp")
  (load "count.lsp")
  (load "count-if.lsp")
  (load "count-if-not.lsp")
  (load "reverse.lsp")
  (load "nreverse.lsp")
  (load "sort.lsp")
  (load "stable-sort.lsp")
  (load "length.lsp")
  (load "find.lsp")
  (load "find-if.lsp")
  (load "find-if-not.lsp")
  (load "position.lsp")
  (load "position-if.lsp")
  (load "position-if-not.lsp")
  (load "search-list.lsp")
  (load "search-vector.lsp")
  (load "search-bitvector.lsp")
  (load "search-string.lsp")
  (load "mismatch.lsp")
  (load "replace.lsp")
  (load "subseq.lsp")
  (load "substitute.lsp")
  (load "substitute-if.lsp")
  (load "substitute-if-not.lsp")
  (load "nsubstitute.lsp")
  (load "nsubstitute-if.lsp")
  (load "nsubstitute-if-not.lsp")
  (load "concatenate.lsp")
  (load "merge.lsp")
  (load "remove.lsp")  ;; also related funs
  (load "remove-duplicates.lsp")  ;; also delete-duplicates
)
