#lang racket/base
(require "vr.rkt")

; Data that will be pre-cached before the first frame is rendered.
; The uri macro defines the name and adds a cache command to the init command list.
(uri WAV-ACTIVATE    "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_activate_01.wav")


;-----------------
; link-button
;-----------------
(define (link-button title height yaw target)
  (define bounds-trans (mat4-compose (mat4-translate -0.5 -0.3 -0.5) 
                                     (mat4-scale/xyz 1.0 0.15 0.15) 
                                     (mat4-translate 0.0 height -2.0)
                                     (mat4-rotate-y yaw)))
  (define gaze-now (gaze-on-bounds? bounds3-unit bounds-trans))
  
  ; Position the text
  (+text title
            (mat4-compose 
             (mat4-scale 2.0) 
             (mat4-translate 0.0 height -2.0)
             (mat4-rotate-y yaw))             
            (if gaze-now 
                (opt-parm 1.0 1.0 0.5 1.0) 
                (opt-parm 0.5 0.5 1.0 1.0)))
  
  ; if an input click just happened and we are gazing on it, change rooms
  (when (and (pressed-action) gaze-now)
      (display (format "Going to ~a\n" target))
      (+sound WAV-ACTIVATE)
      (+link target)))

; returns #f if not found
; scans forward
(define (first-substring-index string substr)
  (define sublen (string-length substr))
  (define stop (- (string-length string) sublen -1))
  (define (scan index)
    (cond
      ((>= index stop) #f)
      ((string=? substr (substring string index (+ index sublen))) index)
      (#t (scan (+ 1 index)))))
  (scan 0))

; returns #f if not found
; scans backwards
(define (last-substring-index string substr)
  (define sublen (string-length substr))
  (define (scan index)
    (cond
      ((< index 0) #f)
      ((string=? substr (substring string index (+ index sublen))) index)
      (#t (scan (- index 1)))))
  (scan (- (string-length string) sublen)))

(define (isScript? s)
  (define ind (last-substring-index s ".rkt"))
  (and ind (= ind (- (string-length s) 4))))


; Extract the last path component without the extension.
; "/subdir/name.tkt" -> "name"
(define (script-label s)
  (define last/ (last-substring-index s "/"))
  (define last. (last-substring-index s "."))
  (if (and last. last/ (> last. last/))
      (substring s (+ 1 last/) last.)
      s))
      
; Adds vrscript:// at the beginning if no :// is present
(define (script-uri s)
  (if (first-substring-index s "://")
      s
      (string-append "vrscript://" s)))

(define (main-links y lst)
  (unless (null? lst)
    (define btn (car lst))
    (link-button (list-ref btn 0) y 0 (list-ref btn 1))
    (main-links (- y 0.2) (cdr lst))))
  
;-----------------
; frame function
;-----------------
(define (frame)
  (+pano "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_lobby.JPG")  

  (main-links 0.75
              '{
               ("Office Tour"  "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.3/office.rkt")
               ("Shader Test"  "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.3/shader.rkt")
               ("Fisheye"      "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.3/gopro.rkt")
               ("Space Needle" "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.3/space-needle.rkt")
               ("3D Audio"     "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.3/voice-around.rkt")
               ("World Tour"   "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.3/tour.rkt")
               ("Reversi Game" "vrscript://s3.amazonaws.com/o.oculuscdn.com/vrscript0.3/reversi.rkt")
               ;  ("Mafia Multi"  "vrscript://s3.amazonaws.com/o.oculuscdn.com/netasset/mafia-multi.rkt")
               })
               
  (+text (format "phone ip: ~a" (init-parm "ip"))
             (mat4-compose 
              (mat4-scale 2.0) 
              (mat4-translate 0.0 -0.75 -2.0)))
  
  ; If the contents of the clipboard look like a script, add a link.
  ; This is a phone-only way to launch any script you want without needing
  ; a web page with a vrscript:// scheme uri or an AppLink.
  (let ((scr (init-parm "clipboard")))
        (when (isScript? scr)
          (link-button (script-label scr) -1.0 (script-uri scr))))

  ; Make link buttons for all the .rkt files in the local /sdcard/Oculus/vrscript directory
  (let ((lst (init-parm "scripts")))
        (for-each (lambda (ind)
                    (define lnk (list-ref lst ind))
                    (link-button (script-label lnk)
                                 (- 0.75 (* 0.2 ind))
                                 (* 0.5 -pi/2)
                                 (string-append "file://" lnk)))
                  (iota (length lst))))                    
  )
  
; This connects to the vrscript.
; Replace the IP address with the value shown on the phone when NetHmd is run for remote
; debugging, it is ignored for embedded execution.
(vrmain "192.168.1.147" frame)
 
