#!r6rs
(import (only (racket base) require))
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

;-----------------
; text-panel
; Bring up a panel of text.
;-----------------
(define (count-newlines txt)
  (define (cnd index so-far)
    (cond
      ((= index -1) so-far)
      (#t (cnd (- index 1) (if (eq? (string-ref txt index) #\newline) (+ 1 so-far) so-far)))))
  (cnd (- (string-length txt) 1) 0))

; Draw a normal blended quad with the background color, then draw a non-blended quad with the
; alpha mask to enable the signed distance field TimeWarp filter.
(define (text-panel txt degree-angle)
  (define lines (+ 1.0 (count-newlines txt)))
  ; normal blended-edge quad that always writes alpha = 1
  (cmd-quad! "_background" 
             (mat4-compose (mat4-translate -0.5 -0.5 0.0)
                           (mat4-scale 1.4) 
                           (mat4-scale/xyz 1.35 (+ 0.1 (* 0.072 lines)) 0.0) 
                           (mat4-translate 0.0 1.6 -3.1) 
                           (mat4-rotate-y (degrees->radians degree-angle)) ) 
             (opt-parm 0.0 0.0 0.0 1.0)
             (opt-blend-ext GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ONE GL_FUNC_ADD GL_FUNC_ADD)
             'depth-mask)
  ; slightly smaller non-blended mask
  (cmd-quad! "_white" 
             (mat4-compose (mat4-translate -0.5 -0.5 0.0)
                           (mat4-scale 1.4) 
                           (mat4-scale/xyz 1.3 (+ 0.05 (* 0.072 lines)) 0.0) 
                           (mat4-translate 0.0 1.6 -3.1) 
                           (mat4-rotate-y (degrees->radians degree-angle)) ) 
             (opt-parm 0.0 0.0 0.0 0.0)
             (opt-blend-ext GL_ONE GL_ZERO GL_ONE GL_ZERO GL_FUNC_ADD GL_FUNC_ADD)
             'depth-mask)
  ; text will blend on top
  (cmd-text-ext! txt TEXT_HORIZONTAL_CENTER TEXT_VERTICAL_CENTER
                 (mat4-compose (mat4-scale 1.4) (mat4-translate 0.0 1.6 -2.95) (mat4-rotate-y (degrees->radians degree-angle)) )))
               

;-----------------
; tic function
;-----------------
(define (tic)
  ; background
  (cmd-pano! "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_lobby.JPG")

  ; text panels
  (text-panel
"Asynchronous resource load options:
1: Freeze time while loading, allow the
current frame to re-render.
2: Force a fade-out, allowing the current
frame to re-render in freeze-time.
3: Ignore commands with unloaded assets"
-50.0)

  (text-panel
"Available only in the fragment shader,
these functions return the partial
derivative of expression p with respect
to the window x coordinate (for dFdx*)
and y coordinate (for dFdy*).

dFdxFine and dFdyFine calculate
derivatives using local differencing
based on on the value of p for the
current fragment and its immediate
neighbor(s).

dFdxCoarse and dFdyCoarse calculate
derivatives using local differencing
based on the value of p for the current
fragment's neighbors, and will
possibly, but not necessarily, include
the value for the current fragment.
That is, over a given area, the
implementation can compute derivatives
in fewer unique locations than would be
allowed for the corresponding dFdxFine
and dFdyFine functions."
0.0)

  (text-panel
"Alan Kay has famously described Lisp as
the \"Maxwell's equations of software\".
He describes the revelation he
experienced when, as a graduate student,
he was studying the LISP 1.5
Programmer's Manual and realized that
\"the half page of code on the bottom of
page 13... was Lisp in itself. These
were \"Maxwell's Equations of Software!\"
This is the whole world of programming
in a few lines that I can put my hand
over.\""
50.0)
    
  ; done
  )
    
; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when NetHmd is run.
; The init function is optional, use #f if not defined.
(remote "172.22.52.94" #f tic)
 