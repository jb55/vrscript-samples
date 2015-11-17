#!r6rs
(import (only (racket base) require))
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

(define *player* 1)
(define *board* (make-vector 64 0))

(uri WAV-ACTIVATE    "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_activate_01.wav")

; Having problems importing SRFI 1
(define (iota stop)
  (define (rev x)
    (if (= x stop)
        '()
        (cons x (rev (+ x 1)))))
  (rev 0))

;--------------- reversi rules -----------------
(define (board-get x y)
  (vector-ref *board* (+ (* y 8) x)))

(define (board-set! x y c)
  (vector-set! *board* (+ (* y 8) x) c))

(define (board)
  (define (row y)
    (for-each (lambda (x) (display (board-get x y))) (iota 8))
    (newline))
  (for-each row (iota 8)))

(define (clear-board)
  (set! *board* (make-vector 64 0))
  (board-set! 3 3 1)
  (board-set! 4 4 1)
  (board-set! 3 4 2)
  (board-set! 4 3 2))

; encode the board 4 rows in each float
(define (board-bits test b stop val)
  (if (= b stop)
      val
      (if (= test (vector-ref *board* b))
          (board-bits test (- b 1) stop (+ (* 2 val) 1))
          (board-bits test (- b 1) stop (* 2 val)))))
  
(define (board->vec4 test)
  (make-vec4 (board-bits test 15 -1 0)
             (board-bits test 31 15 0)
             (board-bits test 47 31 0)
             (board-bits test 63 47 0)))

(clear-board)

(define (legal? x y c)
  (cond
    ((> (board-get x y) 0) #f) ; something else already there
    (else #t)))  ; TODO: must place in a capturing position

; place a color and capture in all directions
(define (place x y c)
  (define take (if (= c 1) 2 1)) ; the color we can capture
  (define (scan-start dx dy)
    (define (flip fx fy)
      (cond
        ((and (= fx x) (= fy y))  #f) ; back to the start point
        (else (board-set! fx fy c)     ; take it and continue flipping back
              (flip (- fx dx) (- fy dy)))))
    (define (scan sx sy)      
      (cond
        ((not (and (< -1 sx 8) (< -1 sy 8))) #f) ; off the edge, didn't find a match
        ((= c (board-get sx sy)) (flip (- sx dx) (- sy dy))) ; we can take this, continue checking
        ((not (= take (board-get sx sy))) #f) ; hit a spot that isn't our take color
        (else (scan (+ sx dx) (+ sy dy)))))   ; this may be flipable, keep looking
    (scan (+ x dx) (+ y dy)))
  (display (format "place: ~a ~a\n" x y))
  (board-set! x y c)
  (scan-start 1 0)
  (scan-start 0 1)
  (scan-start -1 0)
  (scan-start 0 -1)
  (scan-start 1 1)
  (scan-start 1 -1)
  (scan-start -1 1)
  (scan-start -1 -1))
  
;---------------------------------------------

(define board-vertex-shader
"
#version 300 es
uniform highp mat4 Mvpm;
in highp vec4 Position;
in highp vec2 TexCoord;
out highp vec2 oTexCoord;
void main()
{
  oTexCoord = TexCoord;
  gl_Position = Mvpm * Position;
}
")

(cmd-shader! "board" board-vertex-shader
           
"
#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out vec4 gl_FragColor;
void main()
{
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
int c = ( x ^ y ) & 1;
gl_FragColor = vec4( 0.0, 0.2 + 0.1 * float(c) , 0.0, 1.0);
}
")

(cmd-shader! "disc-white" board-vertex-shader
"
#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out vec4 gl_FragColor;
void main()
{
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
int c = ( x ^ y ) & 1;
int isSet;
if ( y < 2) isSet = int(UniformColor.x);
else if ( y < 4) isSet = int(UniformColor.y);
else if ( y < 6) isSet = int(UniformColor.z);
else isSet = int(UniformColor.w);
y &= 3;
vec2 p = fract( oTexCoord * 8.0 ) - 0.5;
float dist = length( p );
float on = (dist < 0.4) ? 1.0 : 0.0;
if ( ((isSet & (1 << ((y<<3) + x))) > 0) && dist < 0.4 )
{
  gl_FragColor = vec4( 1.0, 1.0, 1.0, 1.0 );
}
else
{
  gl_FragColor = vec4( 0.0, 0.0, 0.0, 0.0 );
}
}
")

(cmd-shader! "disc-black" board-vertex-shader
"
#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out vec4 gl_FragColor;
void main()
{
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
int c = ( x ^ y ) & 1;
int isSet;
if ( y < 2) isSet = int(UniformColor.x);
else if ( y < 4) isSet = int(UniformColor.y);
else if ( y < 6) isSet = int(UniformColor.z);
else isSet = int(UniformColor.w);
y &= 3;
vec2 p = fract( oTexCoord * 8.0 ) - 0.5;
float dist = length( p );
float on = (dist < 0.4) ? 1.0 : 0.0;
if ( ((isSet & (1 << ((y<<3) + x))) > 0) && dist < 0.4 )
{
  gl_FragColor = vec4( 0.0, 0.0, 0.0, 1.0 );
}
else
{
  gl_FragColor = vec4( 0.0, 0.0, 0.0, 0.0 );
}
}
")


(cmd-shader! "cursor" board-vertex-shader
"
#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out vec4 gl_FragColor;
void main()
{
float d = length(oTexCoord - UniformColor.xy);
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
int c = ( x ^ y ) & 1;
if ( d < 0.02 )
  gl_FragColor = vec4( UniformColor.z, UniformColor.z, UniformColor.z, 1.0);
else
  gl_FragColor = vec4( 0.0, 0.0, 0.0, 0.0);
}
")

; returns the xy0 intersection on the z=0 plane of xform
(define (intersect-plane start dir xform)
  (define inv (mat4-inverse xform))
  (define lstart (mat4-trans3 inv start))
  (define ldir (mat4-trans-dir inv dir))
  (define spd (safe-rcp (vec3-z ldir)))
  (define pt (sub3 lstart (scale3 ldir (* (vec3-z lstart) spd))))
  (make-vec3 (vec3-x pt) (- 1.0 (vec3-y pt)) 0.0))
  
(define (floor->exact x) (exact (floor x)))

(define (tic)
  (define board-xform (mat4-compose (mat4-rotate-x pi/2)
                                    (mat4-translate -0.5 0.7 -1.6) ))
  (define spot (intersect-plane (mat4-origin *pose-inverse*) (mat4-forward *pose-inverse*) board-xform))
  (if (pressed-action)
      (cond
        ((and (< 0.0 (vec3-x spot) 1.0) (< 0.0 (vec3-y spot) 1.0))
         (let ( (x (floor->exact (* 8 (vec3-x spot))))
                (y (floor->exact (* 8 (vec3-y spot)))) )
         (if (legal? x y *player*)
             (begin
               (place x y *player*)
               (set! *player* (if (= *player* 2) 1 2))
               (cmd-local-sound! WAV-ACTIVATE))
             #f))))
      #f)
    
  (cmd-pano! "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_demo.JPG")
  (cmd-quad! "_white" 
             board-xform
             (opt-parm (vec3-x spot) 
                       (vec3-y spot) 
                       (if (= *player* 2) 1.0 0.0)
                       0.0)
             (opt-shader "cursor"))
  (cmd-quad! "_white" 
             board-xform
             (opt-parmv (board->vec4 2))
             (opt-shader "disc-white"))
  (cmd-quad! "_white" 
             board-xform
             (opt-parmv (board->vec4 1))
             (opt-shader "disc-black"))
  (cmd-quad! "_white" 
             board-xform
             (opt-shader "board")))


; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when NetHmd is run.
; The init function is optional, use #f if not defined.
(remote "172.22.52.94" #f tic)
