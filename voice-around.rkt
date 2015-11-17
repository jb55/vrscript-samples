#lang racket/base
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

(uri WAV-VOICE-TEST  "http://vr.oculuscdn.com/assets/sounds/test/mono_human_voice_test_01b.wav")

(define NAME-VOICE 1)

;-----------------
; tic function
;-----------------
(define (tic)
  (define xform (mat4-compose (mat4-rotate-y pi)
                              (mat4-translate 0.0 0.0 2.0)
                              (mat4-rotate-y (input-time *input*))))
  
  ; select a background
  (+pano "http://vr.oculuscdn.com/assets/panos/office_demo.JPG")

  ; spin a model in front of the view
  (+model "http://vr.oculuscdn.com/assets/models/icon_speaker.ovrscene"
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
(remote "172.22.52.204" #f tic)
 
