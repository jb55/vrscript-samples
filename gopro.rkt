#lang racket/base
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

; should a draw command take an optional element type for lines / points?

; Having problems importing SRFI 1
; iota returns a list from 0 (inclusive) to stop (exclusive)
(define (iota stop)
  (define (rev x)
    (if (= x stop)
        '()
        (cons x (rev (+ x 1)))))
  (rev 0))


;---------------------------------------------------
; Geometry generation
;
; Geo is a stateful "geometry builder"
; Start with (empty-geo), then repeatedly call:
; (geo-add-indexes! <any number of indexes>)
; (geo-add-vertex! <up to GL_MAX_VERTEX_ATTRIBS attributes>)
; Then, finalize it with (+geometry name geo shader)
; The given name can then be used with the (+model name xform)
; command.
;
; (define g (empty-geo))
; (geo-add-vertex! g '(0 0 0)  '(1 0 0))
; (geo-add-vertex! g '(0 10 0) '(0 1 0))
; (geo-add-vertex! g '(10 0 0) '(0 0 1))
; (geo-add-indexes! g 0 1 2)
; (+geometry "rgbTri" g "Vertex-Color")
;
; Vertexes can have up to GL_MAX_VERTEX_ATTRIBS (32 on current hardware)
; attributes, each of which can have 0 to 4 elements.  All vertexes
; must have a consistent set of attributes.
; The attributes can be list of floats, vectors of floats,
; or vec3 / vec4 record types.
;
; Empty attributes are allowed, so you can force data to specific
; attribut indexes for program compatibility:
;
; (geo-add-vertex! geo '(0 10 0) '() '() '() '(0.0 0.5))
;
; #version 300 es vertex shaders can use explicit layout locations to
; address the attributes in a geometry:
;
; layout(location=1) in highp vec3 VertexColor;
;
; The following input attribute names are automatically bound
; by VrAppFramework when loading shaders, you can use these
; names without an explict layout location:
;
; 0 Position
; 1 Normal
; 2 Binormal
; 3 VertexColor
; 4 TexCoord
; 5 TexCoord1
; 6 JointIndices
; 7 JointWeights
; 8 FontParms
;
; Explicit layout locations will override the above bindings,
; so you can freely reuse the names.
;
; The actual parsing of all this is terribly inefficient, so
; use caution with larger models.
;
; TODO: non float sizes and types -- shorts, bytes, integers
;---------------------------------------------------

(define-record-type geo
  (make-geo vertexes indexes)
  geo?
  (vertexes geo-vertexes set-geo-vertexes!)
  (indexes geo-indexes set-geo-indexes!))

(define (geo->string geo)
  (format "(~a ~a)" (geo-indexes geo) (geo-vertexes geo)))

; Start with no indexes and no vertexs
(define (empty-geo)
  (make-geo '() '()))

; These get built up in reverse order
(define (geo-add-indexes! geo . indexes)
  (set-geo-indexes! geo (append (reverse indexes) (geo-indexes geo))))

(define (geo-add-vertex! geo . attributes)
  (define (listify-attribute at)
    (cond
      ((list? at) at)
      ((null? at) at)
      ((vector? at) (vector->list at))
      ((vec3? at) (vec3->list at))
      ((vec4? at) (vec4->list at))
      (#t (error (format "Bad type in listify-attribute: ~a" at)))))
  (define (inexactify at) ; force to flonum so we don't get ratios like 5/8, which C++ won't like
    (map exact->inexact at))  
  (set-geo-vertexes! geo (cons (map inexactify (map listify-attribute attributes)) (geo-vertexes geo))))

(define (+geometry name geo shader)
  (+cmd (list 'geometry name (reverse (geo-indexes geo)) (reverse (geo-vertexes geo)) shader)))

;-------------------------------------------------------------


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
                                                               (make-vec3 (sin angle) 0.0 (* -1.0 (cos angle))))
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

(define (tic)
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
  
  (+set-position (make-vec3 0.0 0.0 0.0) 0.0) 
  (+pano "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_john.JPG")
  (+model "screen"
          (mat4-scale 90.0)
         ; (mat4-translate-z -1.0)
          ; (opt-texture "https://s3.amazonaws.com/o.oculuscdn.com/netasset/fisheye.jpg")
          (opt-texture "https://s3.amazonaws.com/o.oculuscdn.com/netasset/2k-frame0.jpg")
          )
;  (+quad "https://s3.amazonaws.com/o.oculuscdn.com/netasset/fisheye.jpg" (mat4-translate-z -1.0) )

  )

; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when NetHmd is run.
; The init function is optional, use #f if not defined.
(remote "172.22.52.94" #f tic)
 
