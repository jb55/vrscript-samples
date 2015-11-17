#lang racket/base
(require "vr.rkt")


(+shader "board"

"#version 300 es
uniform highp mat4 Mvpm;
in highp vec4 Position;
layout(location=1) in highp vec2 TexCoord;
out highp vec2 oTexCoord;
void main()
{
  oTexCoord = TexCoord;
  gl_Position = Mvpm * Position;
}
"
         
#;"#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out lowp vec4 out_FragColor;
void main()
{
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
int c = ( x ^ y ) & 1;
out_FragColor = vec4( 0.0, 0.2 + 0.1 * float(c) , 0.0, 1.0);
}
"

"#version 300 es
in highp vec2 oTexCoord;
out lowp vec4 out_FragColor;
uniform sampler2D Texture0;
void main()
{
out_FragColor = texture( Texture0, oTexCoord );
}"

)


; Returns a name for a geometry that can be drawn.

(define sensor-width 4096.0)
(define sensor-height 2160.0)
(define corner-half-angle (degrees->radians 80.0))
(define factor 0.5)
(define corner-length (sqrt (+ (* sensor-width sensor-width) (* sensor-height sensor-height))))


(define (distort dist)
  (define d (/ dist corner-length)) ; now in 0.0 to 1.0 range
;  (* corner-half-angle d))
  (* (tan (* factor d)) (* corner-half-angle (/ 1.0 (tan factor)))))

(define (make-screen)
  ; The quads will be evenly spaced by texture coordinate, but the xy vertexes
  ; will be undistorted, giving a pincushion look.
  (define tess 16)  ; number of quads from 0.0 to 1.0
  (define axis (+ 1 (* 2 tess)))
  (define geo (empty-geo))
  (for-each (lambda (y)
              (define fy (/ (- y tess) tess))
              (define sy (* sensor-height fy))
              (for-each (lambda (x)                              
                          (define fx (/ (- x tess) tess))
                          (define sx (* sensor-width fx))
                          (define dist (sqrt (+ (* sx sx) (* sy sy))))
                          (define angle (distort dist))
                          (if (and (= x tess) (= y tess))
                              (geo-add-vertex! geo '(0 0 -1) (list (+ 0.5 (* 0.5 fx)) (+ 0.5 (* -0.5 fy))))
                              (geo-add-vertex! geo
                                               (mat4-transform3 (mat4-rotate-z (atan sy sx))
                                                               (vec3 (sin angle) 0.0 (* -1.0 (cos angle))))
                                               (list (+ 0.5 (* 0.5 fx)) (+ 0.5 (* -0.5 fy))))))
              (iota axis)))
            (iota axis))
  (for-each (lambda (y)
              (for-each (lambda (x)
                          (define i (+ x (* y axis)))
                          (when #t #;(= 0 (bitwise-and 1 (bitwise-xor x y)))
                            (geo-add-indexes! geo i (+ i 1) (+ i axis 1) i (+ i axis 1) (+ i axis))))
                        (iota (- axis 1))))
            (iota (- axis 1)))
  
  geo)

(+geometry "screen" (make-screen) "board")

(define (frame)
  (+hud (format "factor:~a angle:~a" factor corner-half-angle))
  (when (pressed-dpad-up)
    (set! factor (+ factor 0.1))
    (+geometry "screen" (make-screen) "board"))
  (when (pressed-dpad-down)
    (set! factor (- factor 0.1))
    (+geometry "screen" (make-screen) "board"))
  (when (pressed-dpad-right)
    (set! corner-half-angle (+ corner-half-angle 0.1))
    (+geometry "screen" (make-screen) "board"))
  (when (pressed-dpad-left)
    (set! corner-half-angle (- corner-half-angle 0.1))
    (+geometry "screen" (make-screen) "board"))
  
  (+set-position (vec3 0.0 0.0 0.0) 0.0) 
  (+pano "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_john.JPG")
  (+model "screen"
          (mat4-scale 90.0)
         ; (mat4-translate-z -1.0)
          ; (opt-texture "https://s3.amazonaws.com/o.oculuscdn.com/netasset/fisheye.jpg")
          (opt-texture "https://s3.amazonaws.com/o.oculuscdn.com/netasset/2k-frame0.jpg")
          )
;  (+quad "https://s3.amazonaws.com/o.oculuscdn.com/netasset/fisheye.jpg" (mat4-translate-z -1.0) )

  )

(vrmain "172.22.52.41" frame)
 
