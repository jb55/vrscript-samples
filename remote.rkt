#lang racket/base
(require racket/tcp)
(require racket/port)
(require "vr.rkt")
(require ffi/unsafe)

; Try to provide everything that the scripts will use,
; to reduce boilerplate code at the top.
(provide remote
         (all-from-out "vr.rkt")
         )

; from http://macrologist.blogspot.com/2012/03/avoid-flushing-your-wire-protocols.html
(define IPPROTO_TCP 6)
(define TCP_NODELAY 1)
 
(define setsockopt_tcp_nodelay
  (get-ffi-obj "setsockopt" #f
    (_fun (socket enabled?) ::
          (socket : _int)
          (_int = IPPROTO_TCP)
          (_int = TCP_NODELAY)
          (enabled-ptr : (_ptr i _int)
                       = (if enabled? 1 0))
          (_int = (compiler-sizeof 'int))
          -> (result : _int)
          -> (if (zero? result)
                 (void)
                 (error 'set-tcp-nodelay! "failed")))))
 
(define scheme_get_port_socket
  (get-ffi-obj "scheme_get_port_socket" #f
    (_fun (port) ::
          (port : _racket)
          (socket : (_ptr o _intptr))
          -> (result : _int)
          -> (and (positive? result) socket))))
 
; set-tcp-nodelay! : tcp-port boolean -> void
(define (set-tcp-nodelay! port enabled?)
  (let ([socket (scheme_get_port_socket port)])
    (setsockopt_tcp_nodelay socket enabled?)))

;--------------------------------------
; main loop
;--------------------------------------

(define target-phone-port 8008)

(define (loop2 in out tic)
  ; block until a byte is available
;  (define evt (peek-bytes-evt 1 0 #f in))
;  (define start (current-inexact-milliseconds))
;  (sleep 0.008)
;  (sync evt)
;  (define end (current-inexact-milliseconds))
;  (printf "blocked ~a ms\n" (- end start))
  
  ; read an entire s-expression
  (define sexp (read in))
  (write (tic-wrap tic sexp) out)
  (flush-output out)
  (loop2 in out tic))

; Interactively loop for the HMD
;
; The parms s-expression is sent to the target ip-address to
; allow it to be configured for multiplayer development.
;
; For local execution on the phone, the ip-address is forced to "localhost"
(define (remote/parms parms ip-address init tic)
  ; If the ports are set to completely unbuffered, each object in a printf
  ; winds up in a separate message and on a separate logcat line, which we
  ; do not want.
;  (file-stream-buffer-mode (current-output-port) 'none)
;  (file-stream-buffer-mode (current-error-port) 'none)
  ; On Mac, we don't seem to be able to change buffering on the output port
;  (file-stream-buffer-mode (current-output-port) 'line)
;  (file-stream-buffer-mode (current-error-port) 'line)
  
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (printf "connecting\n")
    (define-values (in out) (tcp-connect ip-address target-phone-port))
    (set-tcp-nodelay! out #t)
;    (file-stream-buffer-mode out 'none)
    (file-stream-buffer-mode out 'block)
    (printf "Write configuration: ~a\n" parms )
    (write parms out)
    (flush-output out)
    
    (printf "read init-parms\n")
    (define init-parms (read in))
    
    (printf "run init\n")
    (define init-out (init-wrap init init-parms))
    
    (printf "write init-out\n")
    (write  init-out out)
    (flush-output out)
    
    (printf "\nlooping\n")
    (loop2 in out tic)
    (close-input-port in)
    (close-output-port out)))

; Start a remote session, optionally with server-parms
(define (remote ip-address frame . opts)
  (printf "(system-type 'os) -> ~a\n" (system-type 'os))
  ; server parms are empty unless both init and server parms are specified
  (define server-parms
    (if (= (length opts) 2)
        (append (list 'server) (list-ref opts 1) (list (sexpr->string *server-state*)))
        '()))
  (define init
    (if (>= (length opts) 1)
        (list-ref opts 0)
        #f))
  (remote/parms server-parms ip-address init frame))
