#lang racket/base
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

(uri WAV-VOICE-TEST  "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/mono_human_voice_test_01b.wav")

(define NAME-VOICE 1)

;-----------------
; tic function
;-----------------
(define (tic)
  (define xform (mat4-compose (mat4-rotate-y pi)
                              (mat4-translate 0.0 0.0 -1.0)
                              (mat4-rotate-y (input-time *input*))))
  
  ; select a background
  (+pano "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_lobby.JPG")

  ; spin a model in front of the view
  (+model "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/head-0.ovrscene"
              xform)

  ; update the audio position
  (+sound WAV-VOICE-TEST
              (opt-name NAME-VOICE)
              'loop
              (opt-position (mat4-origin xform)))
  )

; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when vrscript is run.
; The init function is optional, use #f if not defined.
(remote "172.22.52.94" #f tic)
 
