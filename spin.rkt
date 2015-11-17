#lang racket/base
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

(define *tex* 0)

(define textures
  '("file:///sdcard/temp/basketball.tga"
    "file:///sdcard/temp/volleyball.tga"
    "file:///sdcard/temp/baseball.tga"))

;-----------------
; tic function
;-----------------
(define (tic)
  ; select a background
  (+pano "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_lobby.JPG")

  ; user swapping of texture
  (if (pressed-action)
      (set! *tex* (remainder (+ 1 *tex*) 3))
      #f)
  
  ; spin a model in front of the view
;  (cmd-model! "https://s3.amazonaws.com/o.oculuscdn.com/netasset/ball.ovrscene"
  (+model "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/head-0.ovrscene"
              (mat4-compose (mat4-rotate-y (input-time *input*))
                            (mat4-translate 0.0 0.0 -1.0))
;              (opt-texture (list-ref textures *tex*))
              )
  
  ; add a text label 
  )

; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when vrscript is run.
; The init function is optional, use #f if not defined.
(remote "172.22.52.94" #f tic)
;(remote "172.22.52.248" #f tic)
 
