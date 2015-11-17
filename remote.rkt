#lang racket/base
(require racket/tcp)
(require "vr.rkt")
(require rnrs/lists-6)
(require rnrs/arithmetic/bitwise-6)
(require rnrs/base-6)
(require srfi/9)
;(require (except-in srfi/1 pair? map for-each))

; Try to provide everything that the scripts will use,
; to reduce boilerplate code at the top.
(provide remote
         (all-from-out "vr.rkt")
         (all-from-out rnrs/base-6)
         (all-from-out rnrs/arithmetic/bitwise-6)
         (all-from-out rnrs/lists-6)
         (all-from-out srfi/9)       ; define-record-type
         display newline write  ;  (rnrs io simple (6))
         format                 ; (srfi :28) ; format
         )

; This code is only executed by DrRacket on a PC to allow the VR scheme code
; to be debugged while communicating with a Gear VR over TCP.

;--------------------------------------
; main loop
;--------------------------------------

(define target-phone-port 8008)

(define (loop2 in out tic)
  (define sexp (read in))
  (write (tic-wrap tic sexp) out)  
  (loop2 in out tic))

; Interactively loop for the HMD
(define (remote ip-address init tic)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (displayln "connecting")
    (define-values (in out) (tcp-connect ip-address target-phone-port))
    (file-stream-buffer-mode out 'none)
    (displayln "init")
    (define init-parms (read in))
    (write (init-wrap init init-parms) out)
    (displayln "looping")
    (loop2 in out tic)
    (close-input-port in)
    (close-output-port out)))
  
