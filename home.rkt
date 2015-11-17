#lang racket/base
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

; Data that will be pre-cached before the first frame is rendered.
; The uri macro defines the name and adds a cache command to the init command list.
(uri WAV-ACTIVATE    "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_activate_01.wav")


;-----------------
; link-button
; Primary navigation tool.
;-----------------
(define (link-button title height target)
  (define bounds-trans (mat4-compose (mat4-translate -0.5 -0.3 -0.5) 
                                     (mat4-scale/xyz 1.0 0.15 0.15) 
                                     (mat4-translate 0.0 height -2.0) 
                                     ))
  (define gaze-now (gaze-on-bounds? bounds3-unit bounds-trans))
  
  ; Position the text
  (+text title
            (mat4-compose 
             (mat4-scale 2.0) 
             (mat4-translate 0.0 height -2.0))
            (if gaze-now 
                (opt-parm 1.0 1.0 0.5 1.0) 
                (opt-parm 0.5 0.5 1.0 1.0)))
  
  ; if an input click just happened and we are gazing on it, change rooms
  (when (and (pressed-action) gaze-now)
      (display (format "Going to ~a\n" target))
      (+sound WAV-ACTIVATE)
      (+link target)))

;-----------------
; tic function
;-----------------
(define (tic)
  (+pano "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_lobby.JPG")  
  
  (link-button "Office Tour" 0.75 "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.2/office.rkt")
;  (link-button "Shader Test" 0.5 "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.2/shader.rkt")
  (link-button "Fisheye" 0.5 "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.2/gopro.rkt")
  (link-button "3D Audio" 0.25 "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.2/voice-around.rkt")
  (link-button "World Tour" 0.0 "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.2/tour.rkt")
  (link-button "Reversi Game" -0.25 "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.2/reversi.rkt")
  (link-button "Mafia Multi" -0.5 "vrscript://s3.amazonaws.com/o.oculuscdn.com/netasset/mafia-multi.rkt")
  
  (+text (format "phone ip: ~a" (init-parm "ip"))
             (mat4-compose 
              (mat4-scale 2.0) 
              (mat4-translate 0.0 -0.75 -2.0)))
  )

; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when NetHmd is run.
; The init function is optional, use #f if not defined.
(remote "172.22.52.94" #f tic)
 
