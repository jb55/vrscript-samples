#!r6rs
(import (only (racket base) require))
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

;-----------------
; tic function
;-----------------
(define (tic)
  ; select a background
  (cmd-pano! "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_lobby.JPG")
  
  ; spin a model in front of the view
  (cmd-model! "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/head-0.ovrscene"
              (mat4-compose (mat4-rotate-y (input-time *input*))
                            (mat4-translate 0.0 1.7 -1.0)))
  
  ; add a text label 
  )

; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when NetHmd is run.
; The init function is optional, use #f if not defined.
(remote "172.22.52.94" #f tic)
 
