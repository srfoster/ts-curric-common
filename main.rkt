#lang racket

(provide
 (all-from-out "quest-lang.rkt")
 (all-from-out "half-sheets.rkt")
 (all-from-out "k-2-sheets.rkt")
 (all-from-out "k-2-sheet-helpers.rkt")
 (all-from-out "materials.rkt")
 (all-from-out "fleet-winter-2017.rkt")
 (all-from-out "file-code-extraction.rkt")

 #%module-begin)

(require "quest-lang.rkt")
(require "half-sheets.rkt")
(require "k-2-sheets.rkt")
(require "k-2-sheet-helpers.rkt")
(require "materials.rkt")
(require "fleet-winter-2017.rkt")
(require "file-code-extraction.rkt")


;We do some one-time setup by
;  moving the launch script into the right
;  spot...
(require racket/runtime-path)
(define-runtime-path qs "quickscript")

(define src
  (build-path qs "launch.rkt"))

(define dest
  (build-path 
    (find-system-path 'pref-dir) 
    "quickscript"
    "user-scripts"
    "launch.rkt"))

(define setup-successful
  (and #;(not (file-exists? dest)) ;Even if it does exist.  We want it to overwrite, so we can update the launch tool
       (make-directory* (build-path (find-system-path 'pref-dir) "quickscript" "user-scripts"))
       (copy-file src dest #t)))

