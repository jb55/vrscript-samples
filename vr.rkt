#lang racket/base
; TODO: change things to be more r7rs like?

; Everything here is exported
(provide (all-defined-out))
(require srfi/9)  ; define-record-type

; everything up to this mark will be stripped and replaced when embedded
; to work around compatibility issues 
; %%%END-OF-HEADER%%%


(display "start of file\n")

;------------------------------------------------------------------

(define pi 3.141592657)
(define pi/2 (/ 3.141592657 2.0))
(define pi*2 (* 3.141592657 2.0))
  
(define (degrees->radians d) (* d (/ pi 180.0)))
(define (radians->degrees r) (* r (/ 180.0 pi)))


; from OVR_Math.h, correct for float, should be different for Flonum, which is double
(define SmallestNonDenormal 1.1754943508222875e-038)  ; ( 1U << 23 )
(define HugeNumber 1.8446742974197924e+019)           ; ( ( ( 127U * 3 / 2 ) << 23 ) | ( ( 1 << 23 ) - 1 ) )

(define (safe-rcp v)
  (if (> (abs v) SmallestNonDenormal)
      (/ 1.0 v)
      HugeNumber))

; types
(define-record-type vec3 (make-vec3 x y z) vec3? (x vec3-x) (y vec3-y) (z vec3-z))
(define-record-type vec4 (make-vec4 x y z w) vec4? (x vec4-x) (y vec4-y) (z vec4-z) (w vec4-w))
(define-record-type mat4 (make-mat4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)
  mat4? 
  (m00 mat4-m00) (m01 mat4-m01) (m02 mat4-m02) (m03 mat4-m03)
  (m10 mat4-m10) (m11 mat4-m11) (m12 mat4-m12) (m13 mat4-m13)
  (m20 mat4-m20) (m21 mat4-m21) (m22 mat4-m22) (m23 mat4-m23)
  (m30 mat4-m30) (m31 mat4-m31) (m32 mat4-m32) (m33 mat4-m33))
  
(define-record-type bounds3 (make-bounds3 min max) bounds3? (min bounds3-min) (max bounds3-max))


; Type constants
(define org3 (make-vec3 0.0 0.0 0.0))
(define mat4-identity (make-mat4 1.0 0.0 0.0 0.0   0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))
(define bounds3-quad (make-bounds3 (make-vec3 -1.0 -1.0 0.0) (make-vec3 1.0 1.0 0.0)))
(define bounds3-unit (make-bounds3 (make-vec3 0.0 0.0 0.0) (make-vec3 1.0 1.0 1.0)))

; Functions
(define (cross a b) (make-vec3 (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
                                        (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
                                        (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b)))))

(define (vec3->list v) (list (vec3-x v) (vec3-y v) (vec3-z v)))
(define (list->vec3 l) (make-vec3 (list-ref l 0) (list-ref l 1) (list-ref l 2)))
                    
(define (vec4->list v) (list (vec4-x v) (vec4-y v) (vec4-z v) (vec4-w v))) 
(define (list->vec4 l) (make-vec4 (list-ref l 0) (list-ref l 1) (list-ref l 2) (list-ref l 3)))
                    
(define (mat4->list m) (list
                        (mat4-m00 m) (mat4-m01 m) (mat4-m02 m) (mat4-m03 m)
                        (mat4-m10 m) (mat4-m11 m) (mat4-m12 m) (mat4-m13 m)
                        (mat4-m20 m) (mat4-m21 m) (mat4-m22 m) (mat4-m23 m)
                        (mat4-m30 m) (mat4-m31 m) (mat4-m32 m) (mat4-m33 m)))
                        
(define (list->mat4 l) (make-mat4
                                            (list-ref l 0) (list-ref l 1) (list-ref l 2) (list-ref l 3)
                                            (list-ref l 4) (list-ref l 5) (list-ref l 6) (list-ref l 7)
                                            (list-ref l 8) (list-ref l 9) (list-ref l 10) (list-ref l 11)
                                            (list-ref l 12) (list-ref l 13) (list-ref l 14) (list-ref l 15)))
                          
(define (mat4-translate x y z) (make-mat4 1.0 0.0 0.0 x   0.0 1.0 0.0 y   0.0 0.0 1.0 z   0.0 0.0 0.0 1.0))
(define (mat4-translate-x x) (make-mat4 1.0 0.0 0.0 x   0.0 1.0 0.0 0.0   0.0 0.0 1.0 0.0   0.0 0.0 0.0 1.0))
(define (mat4-translate-y y) (make-mat4 1.0 0.0 0.0 0.0   0.0 1.0 0.0 y   0.0 0.0 1.0 0.0   0.0 0.0 0.0 1.0))
(define (mat4-translate-z z) (make-mat4 1.0 0.0 0.0 0.0   0.0 1.0 0.0 0.0   0.0 0.0 1.0 z   0.0 0.0 0.0 1.0))
(define (mat4-translatev v) (make-mat4 1.0 0.0 0.0 (vec3-x v)   0.0 1.0 0.0 (vec3-y v)   0.0 0.0 1.0 (vec3-z v)   0.0 0.0 0.0 1.0))
(define (mat4-scale s) (make-mat4 s 0.0 0.0 0.0 0.0 s 0.0 0.0 0.0 0.0 s 0.0 0.0 0.0 0.0 1.0))
(define (mat4-scale/xyz x y z) (make-mat4 x 0.0 0.0 0.0 0.0 y 0.0 0.0 0.0 0.0 z 0.0 0.0 0.0 0.0 1.0))

(define (mat4-rotate-x radians) 
  (define c (cos radians))
  (define s (sin radians))
  (make-mat4 1.0 0.0 0.0 0.0   0.0 c (- 0.0 s) 0.0  0.0 s c 0.0  0.0 0.0 0.0 1.0))

(define (mat4-rotate-y radians) 
  (define c (cos radians))
  (define s (sin radians))
  (make-mat4 c 0.0 s 0.0   0.0 1.0 0.0 0.0  (- 0.0 s) 0.0 c 0.0  0.0 0.0 0.0 1.0))

(define (mat4-rotate-z radians) 
  (define c (cos radians))
  (define s (sin radians))
  (make-mat4 c (- 0.0 s) 0.0 0.0   s c 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0))

; Transform a vector by a matrix

; A direction is just transformed by the inner 3x3, ignoring translation.  Should we still divide by w?
(define (mat4-trans-dir m v) (make-vec3 (+ (* (mat4-m00 m) (vec3-x v)) (* (mat4-m01 m) (vec3-y v)) (* (mat4-m02 m) (vec3-z v)))
                                                     (+ (* (mat4-m10 m) (vec3-x v)) (* (mat4-m11 m) (vec3-y v)) (* (mat4-m12 m) (vec3-z v)))
                                                     (+ (* (mat4-m20 m) (vec3-x v)) (* (mat4-m21 m) (vec3-y v)) (* (mat4-m22 m) (vec3-z v)))))
(define (mat4-trans3 m v) (make-vec3 (+ (* (mat4-m00 m) (vec3-x v)) (* (mat4-m01 m) (vec3-y v)) (* (mat4-m02 m) (vec3-z v)) (mat4-m03 m))
                                                  (+ (* (mat4-m10 m) (vec3-x v)) (* (mat4-m11 m) (vec3-y v)) (* (mat4-m12 m) (vec3-z v)) (mat4-m13 m))
                                                  (+ (* (mat4-m20 m) (vec3-x v)) (* (mat4-m21 m) (vec3-y v)) (* (mat4-m22 m) (vec3-z v)) (mat4-m23 m))))
(define (mat4-trans4 m v) (make-vec4 (+ (* (mat4-m00 m) (vec4-x v)) (* (mat4-m01 m) (vec4-y v)) (* (mat4-m02 m) (vec4-z v)) (* (mat4-m03 m) (vec4-w v)))
                                                  (+ (* (mat4-m10 m) (vec4-x v)) (* (mat4-m11 m) (vec4-y v)) (* (mat4-m12 m) (vec4-z v)) (* (mat4-m13 m) (vec4-w v)))
                                                  (+ (* (mat4-m20 m) (vec4-x v)) (* (mat4-m21 m) (vec4-y v)) (* (mat4-m22 m) (vec4-z v)) (* (mat4-m23 m) (vec4-w v)))
                                                  (+ (* (mat4-m30 m) (vec4-x v)) (* (mat4-m31 m) (vec4-y v)) (* (mat4-m32 m) (vec4-z v)) (* (mat4-m33 m) (vec4-w v)))))
  
; Matrix multiplication.  A vector will be transformed by the second matrix, then the first, when transformed by their product.
(define (mat4-mul m1 m2) (make-mat4
                                (+ (* (mat4-m00 m1) (mat4-m00 m2)) (* (mat4-m01 m1) (mat4-m10 m2)) (* (mat4-m02 m1) (mat4-m20 m2)) (* (mat4-m03 m1) (mat4-m30 m2)))
                                (+ (* (mat4-m00 m1) (mat4-m01 m2)) (* (mat4-m01 m1) (mat4-m11 m2)) (* (mat4-m02 m1) (mat4-m21 m2)) (* (mat4-m03 m1) (mat4-m31 m2)))
                                (+ (* (mat4-m00 m1) (mat4-m02 m2)) (* (mat4-m01 m1) (mat4-m12 m2)) (* (mat4-m02 m1) (mat4-m22 m2)) (* (mat4-m03 m1) (mat4-m32 m2)))
                                (+ (* (mat4-m00 m1) (mat4-m03 m2)) (* (mat4-m01 m1) (mat4-m13 m2)) (* (mat4-m02 m1) (mat4-m23 m2)) (* (mat4-m03 m1) (mat4-m33 m2)))

                                (+ (* (mat4-m10 m1) (mat4-m00 m2)) (* (mat4-m11 m1) (mat4-m10 m2)) (* (mat4-m12 m1) (mat4-m20 m2)) (* (mat4-m13 m1) (mat4-m30 m2)))
                                (+ (* (mat4-m10 m1) (mat4-m01 m2)) (* (mat4-m11 m1) (mat4-m11 m2)) (* (mat4-m12 m1) (mat4-m21 m2)) (* (mat4-m13 m1) (mat4-m31 m2)))
                                (+ (* (mat4-m10 m1) (mat4-m02 m2)) (* (mat4-m11 m1) (mat4-m12 m2)) (* (mat4-m12 m1) (mat4-m22 m2)) (* (mat4-m13 m1) (mat4-m32 m2)))
                                (+ (* (mat4-m10 m1) (mat4-m03 m2)) (* (mat4-m11 m1) (mat4-m13 m2)) (* (mat4-m12 m1) (mat4-m23 m2)) (* (mat4-m13 m1) (mat4-m33 m2)))
                                
                                (+ (* (mat4-m20 m1) (mat4-m00 m2)) (* (mat4-m21 m1) (mat4-m10 m2)) (* (mat4-m22 m1) (mat4-m20 m2)) (* (mat4-m23 m1) (mat4-m30 m2)))
                                (+ (* (mat4-m20 m1) (mat4-m01 m2)) (* (mat4-m21 m1) (mat4-m11 m2)) (* (mat4-m22 m1) (mat4-m21 m2)) (* (mat4-m23 m1) (mat4-m31 m2)))
                                (+ (* (mat4-m20 m1) (mat4-m02 m2)) (* (mat4-m21 m1) (mat4-m12 m2)) (* (mat4-m22 m1) (mat4-m22 m2)) (* (mat4-m23 m1) (mat4-m32 m2)))
                                (+ (* (mat4-m20 m1) (mat4-m03 m2)) (* (mat4-m21 m1) (mat4-m13 m2)) (* (mat4-m22 m1) (mat4-m23 m2)) (* (mat4-m23 m1) (mat4-m33 m2)))
                                
                                (+ (* (mat4-m30 m1) (mat4-m00 m2)) (* (mat4-m31 m1) (mat4-m10 m2)) (* (mat4-m32 m1) (mat4-m20 m2)) (* (mat4-m33 m1) (mat4-m30 m2)))                                
                                (+ (* (mat4-m30 m1) (mat4-m01 m2)) (* (mat4-m31 m1) (mat4-m11 m2)) (* (mat4-m32 m1) (mat4-m21 m2)) (* (mat4-m33 m1) (mat4-m31 m2)))
                                (+ (* (mat4-m30 m1) (mat4-m02 m2)) (* (mat4-m31 m1) (mat4-m12 m2)) (* (mat4-m32 m1) (mat4-m22 m2)) (* (mat4-m33 m1) (mat4-m32 m2)))
                                (+ (* (mat4-m30 m1) (mat4-m03 m2)) (* (mat4-m31 m1) (mat4-m13 m2)) (* (mat4-m32 m1) (mat4-m23 m2)) (* (mat4-m33 m1) (mat4-m33 m2)))
                                ))

; Matrix composition
; Multiply an arbitrary number of matrices together so that a vertex transformed by the result
; would have been transformed by the given matrices from left to right.  This is the opposite
; order from an OVR_math binary multiply sequence:  a * b * c == (mat4-compose c b a).
;
; (mat4-compose a b) == (mat4-mul b a)
; (mat4-compose a b c) == (mat4-mul c (mat4-mul b a))
; etc

(define (mat4-compose-list mats accum)
  (cond
    ((null? mats) accum)
    (#t (mat4-compose-list (cdr mats) (mat4-mul (car mats) accum)))))
  
;(: mat4-compose (-> mat4 * mat4))
(define (mat4-compose . mats)
  (mat4-compose-list (cdr mats) (car mats)))
  

; http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm
(define (mat4-inverse m)
  (let ( (m00 (mat4-m00 m)) (m01 (mat4-m01 m)) (m02 (mat4-m02 m)) (m03 (mat4-m03 m))
         (m10 (mat4-m10 m)) (m11 (mat4-m11 m)) (m12 (mat4-m12 m)) (m13 (mat4-m13 m))
         (m20 (mat4-m20 m)) (m21 (mat4-m21 m)) (m22 (mat4-m22 m)) (m23 (mat4-m23 m))
         (m30 (mat4-m30 m)) (m31 (mat4-m31 m)) (m32 (mat4-m32 m)) (m33 (mat4-m33 m)) )
    (define det (+
      (* m03 m12 m21 m30) (* -1.0 m02 m13 m21 m30) (* -1.0 m03 m11 m22 m30) (* m01 m13 m22 m30)
      (* m02 m11 m23 m30) (* -1.0 m01 m12 m23 m30) (* -1.0 m03 m12 m20 m31) (* m02 m13 m20 m31)
      (* m03 m10 m22 m31) (* -1.0 m00 m13 m22 m31) (* -1.0 m02 m10 m23 m31) (* m00 m12 m23 m31)
      (* m03 m11 m20 m32) (* -1.0 m01 m13 m20 m32) (* -1.0 m03 m10 m21 m32) (* m00 m13 m21 m32)
      (* m01 m10 m23 m32) (* -1.0 m00 m11 m23 m32) (* -1.0 m02 m11 m20 m33) (* m01 m12 m20 m33)
      (* m02 m10 m21 m33) (* -1.0 m00 m12 m21 m33) (* -1.0 m01 m10 m22 m33) (* m00 m11 m22 m33) ))
     (define oodet (safe-rcp det))
    (make-mat4
     (* oodet (+ (* m12 m23 m31) (* -1.0 m13 m22 m31) (* m13 m21 m32) (* -1.0 m11 m23 m32) (* -1.0 m12 m21 m33) (* m11 m22 m33)))
     (* oodet (+ (* m03 m22 m31) (* -1.0 m02 m23 m31) (* -1.0 m03 m21 m32) (* m01 m23 m32) (* m02 m21 m33) (* -1.0 m01 m22 m33)))
     (* oodet (+ (* m02 m13 m31) (* -1.0 m03 m12 m31) (* m03 m11 m32) (* -1.0 m01 m13 m32) (* -1.0 m02 m11 m33) (* m01 m12 m33)))
     (* oodet (+ (* m03 m12 m21) (* -1.0 m02 m13 m21) (* -1.0 m03 m11 m22) (* m01 m13 m22) (* m02 m11 m23) (* -1.0 m01 m12 m23)))
     (* oodet (+ (* m13 m22 m30) (* -1.0 m12 m23 m30) (* -1.0 m13 m20 m32) (* m10 m23 m32) (* m12 m20 m33) (* -1.0 m10 m22 m33)))
     (* oodet (+ (* m02 m23 m30) (* -1.0 m03 m22 m30) (* m03 m20 m32) (* -1.0 m00 m23 m32) (* -1.0 m02 m20 m33) (* m00 m22 m33)))
     (* oodet (+ (* m03 m12 m30) (* -1.0 m02 m13 m30) (* -1.0 m03 m10 m32) (* m00 m13 m32) (* m02 m10 m33) (* -1.0 m00 m12 m33)))
     (* oodet (+ (* m02 m13 m20) (* -1.0 m03 m12 m20) (* m03 m10 m22) (* -1.0 m00 m13 m22) (* -1.0 m02 m10 m23) (* m00 m12 m23)))
     (* oodet (+ (* m11 m23 m30) (* -1.0 m13 m21 m30) (* m13 m20 m31) (* -1.0 m10 m23 m31) (* -1.0 m11 m20 m33) (* m10 m21 m33)))
     (* oodet (+ (* m03 m21 m30) (* -1.0 m01 m23 m30) (* -1.0 m03 m20 m31) (* m00 m23 m31) (* m01 m20 m33) (* -1.0 m00 m21 m33)))
     (* oodet (+ (* m01 m13 m30) (* -1.0 m03 m11 m30) (* m03 m10 m31) (* -1.0 m00 m13 m31) (* -1.0 m01 m10 m33) (* m00 m11 m33)))
     (* oodet (+ (* m03 m11 m20) (* -1.0 m01 m13 m20) (* -1.0 m03 m10 m21) (* m00 m13 m21) (* m01 m10 m23) (* -1.0 m00 m11 m23)))
     (* oodet (+ (* m12 m21 m30) (* -1.0 m11 m22 m30) (* -1.0 m12 m20 m31) (* m10 m22 m31) (* m11 m20 m32) (* -1.0 m10 m21 m32)))
     (* oodet (+ (* m01 m22 m30) (* -1.0 m02 m21 m30) (* m02 m20 m31) (* -1.0 m00 m22 m31) (* -1.0 m01 m20 m32) (* m00 m21 m32)))
     (* oodet (+ (* m02 m11 m30) (* -1.0 m01 m12 m30) (* -1.0 m02 m10 m31) (* m00 m12 m31) (* m01 m10 m32) (* -1.0 m00 m11 m32)))
     (* oodet (+ (* m01 m12 m20) (* -1.0 m02 m11 m20) (* m02 m10 m21) (* -1.0 m00 m12 m21) (* -1.0 m01 m10 m22) (* m00 m11 m22))))))

 
(define (dot3 a b) (+ (* (vec3-x a) (vec3-x b)) (* (vec3-y a) (vec3-y b)) (* (vec3-z a) (vec3-z b))))
(define (add3 a b) (make-vec3 (+ (vec3-x a) (vec3-x b)) (+ (vec3-y a) (vec3-y b)) (+ (vec3-z a) (vec3-z b))))
(define (sub3 a b) (make-vec3 (- (vec3-x a) (vec3-x b)) (- (vec3-y a) (vec3-y b)) (- (vec3-z a) (vec3-z b))))
(define (mul3 a b) (make-vec3 (* (vec3-x a) (vec3-x b)) (* (vec3-y a) (vec3-y b)) (* (vec3-z a) (vec3-z b))))
(define (neg3 a) (make-vec3 (- 0.0 (vec3-x a)) (- 0.0 (vec3-y a)) (- 0.0 (vec3-z a))))
(define (scale3 a s) (make-vec3 (* (vec3-x a) s) (* (vec3-y a) s) (* (vec3-z a) s)))

(define (vec3-length v) (sqrt (dot3 v v)))
(define (vec3-normalize v) (scale3 v (/ 1.0 (vec3-length v))))
(define (vec3->string v) (format "(~a ~a ~a)" (vec3-x v) (vec3-y v) (vec3-z v)))

; Extract information from a mat4 model matrix
(define (mat4-origin m) (make-vec3 (mat4-m03 m) (mat4-m13 m) (mat4-m23 m)))
(define (mat4-forward m) (mat4-trans-dir m (make-vec3 0.0 0.0 -1.0)))


; Straight port from ModelTrace.cpp
; returns multiple values: hit, t0, t1
(define (intersect-ray-bounds start dir mins maxs)
  (define rcpDir (make-vec3 (safe-rcp (vec3-x dir)) (safe-rcp (vec3-y dir)) (safe-rcp (vec3-z dir))))
  (define s (mul3 (sub3 mins start) rcpDir))
  (define t (mul3 (sub3 maxs start) rcpDir))
  (define minHit (make-vec3 (min (vec3-x s) (vec3-x t)) (min (vec3-y s) (vec3-y t)) (min (vec3-z s) (vec3-z t))))
  (define maxHit (make-vec3 (max (vec3-x s) (vec3-x t)) (max (vec3-y s) (vec3-y t)) (max (vec3-z s) (vec3-z t))))
  (define t0 (max (vec3-x minHit) (vec3-y minHit) (vec3-z minHit)))
  (define t1 (min (vec3-x maxHit) (vec3-y maxHit) (vec3-z maxHit)))
  (list (<= t0 t1) t0 t1))


;-------------------------------------------------------

; Orients Z along the line to viewer, up as close to +Y as possible, and X derived
(define (sprite-matrix sprite-org view-org)
  (define z (vec3-normalize (sub3 view-org sprite-org)))
  (define y (sub3 (make-vec3 0.0 1.0 0.0) (scale3 z (vec3-y z))))
  (define x (cross z y))
  (make-mat4 (vec3-x x) (vec3-y x) (vec3-z x)  (vec3-x sprite-org)
        (vec3-x y) (vec3-y y) (vec3-z y)  (vec3-y sprite-org)
        (vec3-x z) (vec3-y z) (vec3-z z)  (vec3-z sprite-org)
        0.0 0.0 0.0 1.0))
 
; Orients Z parallel with the view

;-------------------------------------------------------
; sys commands write to the communications channel
;-------------------------------------------------------

; A list of s-expressions that will be compressed and sent to the HMD.
(define *frame-commands* '())

(define (cmd! s) 
  (set! *frame-commands* (cons s *frame-commands*)))
  
;------------------------------------------------------------

(define TEXT_HORIZONTAL_LEFT 0)
(define TEXT_HORIZONTAL_CENTER 1)
(define TEXT_HORIZONTAL_RIGHT 2)

(define TEXT_VERTICAL_BASELINE 0)      ; align text by baseline of first row
(define TEXT_VERTICAL_CENTER 1)
(define TEXT_VERTICAL_CENTER_FIXEDHEIGHT 2) ; ignores ascenders/descenders
(define TEXT_VERTICAL_TOP 3)

; Specify size as font height?
(define (cmd-text! txt xform . options) (cmd! (append (list 'text txt TEXT_HORIZONTAL_CENTER TEXT_VERTICAL_BASELINE (mat4->list xform)) options)))
(define (cmd-text-ext! txt horiz vert xform . options) (cmd! (append (list 'text txt horiz vert (mat4->list xform)) options)))

; Creates a textured quad that goes from -1.0 -1.0 0.0 to 1.0 1.0 0.0 with texture
; coordinates that go from 0.0 0.0 to 1.0 1.0.
; The front face of the quad is the +Z side, so translating it by 0.0 0.0 -1.0 will
; make it occuppy a full 90 degree view from the origin with the expected orientation.

(define (cmd-quad! uri xform . options) (cmd! (append (list 'quad uri (mat4->list xform)) options)))

; Only yaw orientations make sense for most panos.
(define (cmd-pano! uri) (cmd! (list 'pano uri)))

(define (cmd-model! uri xform . options) (cmd! (append (list 'model uri (mat4->list xform)) options)))

(define (cmd-sound! uri origin) (cmd! (list 'sound uri (vec3->list origin))))

; A local sound will not be 3D spatialized
(define (cmd-local-sound! uri) (cmd! (list 'sound uri)))
  
; fractions-of-fade per second
(define (cmd-fade! velocity) (cmd! (list 'fade velocity)))

; Shaders can be defined at any time, then referenced with a shader parm on a surface.
(define (cmd-shader! name vertex fragment) (cmd! (list 'shader name vertex fragment)))

(define (cmd-gaze-cursor! distance) (cmd! (list 'gaze-cursor distance)))

; Load and run a new script, optionally passing an s-expression to the new init function.
(define (cmd-link! uri . parms) (cmd! (append (list 'link uri) parms)))

;--------------------------------------
; blending and other surface options
;--------------------------------------
 
; BlendingFactorDest
(define GL_ZERO                                          0)
(define GL_ONE                                           1)
(define GL_SRC_COLOR                                     #x0300)
(define GL_ONE_MINUS_SRC_COLOR                           #x0301)
(define GL_SRC_ALPHA                                     #x0302)
(define GL_ONE_MINUS_SRC_ALPHA                           #x0303)
(define GL_DST_ALPHA                                     #x0304)
(define GL_ONE_MINUS_DST_ALPHA                           #x0305)

; BlendingFactorSrc 
;      GL_ZERO
;      GL_ONE
(define GL_DST_COLOR                                     #x0306)
(define GL_ONE_MINUS_DST_COLOR                           #x0307)
(define GL_SRC_ALPHA_SATURATE                            #x0308)
;      GL_SRC_ALPHA
;      GL_ONE_MINUS_SRC_ALPHA
;      GL_DST_ALPHA
;      GL_ONE_MINUS_DST_ALPHA

; BlendEquationSeparate
(define GL_FUNC_ADD                                      #x8006)
(define GL_BLEND_EQUATION                                #x8009)
(define GL_BLEND_EQUATION_RGB                            #x8009)   ; same as BLEND_EQUATION
(define GL_BLEND_EQUATION_ALPHA                          #x883D)

; BlendSubtract
(define GL_FUNC_SUBTRACT                                 #x800A)
(define GL_FUNC_REVERSE_SUBTRACT                         #x800B)

; Separate Blend Functions
(define GL_BLEND_DST_RGB                                 #x80C8)
(define GL_BLEND_SRC_RGB                                 #x80C9)
(define GL_BLEND_DST_ALPHA                               #x80CA)
(define GL_BLEND_SRC_ALPHA                               #x80CB)
(define GL_CONSTANT_COLOR                                #x8001)
(define GL_ONE_MINUS_CONSTANT_COLOR                      #x8002)
(define GL_CONSTANT_ALPHA                                #x8003)
(define GL_ONE_MINUS_CONSTANT_ALPHA                      #x8004)
(define GL_BLEND_COLOR                                   #x8005)

; TODO: BlendColor support?

; Override the default blend mode on a surface
(define (opt-blend-ext srcRGB destRGB srcAlpha dstAlpha modeRGB modeAlpha) (list 'blend srcRGB destRGB srcAlpha dstAlpha modeRGB modeAlpha))
(define (opt-blend src dst) (opt-blend-ext src dst src dst GL_FUNC_ADD GL_FUNC_ADD))
  
; Arbitrary uniform parameter for surfaces to override the default 1.0 1.0 1.0 1.0
(define (opt-parm x y z w) (list 'parm x y z w))
(define (opt-parmv p) (cons 'parm (vec4->list p)))
  
; Shader override parm
(define (opt-shader p) (list 'shader p))

;--------------------------------------
; input
;--------------------------------------
 
; The input structure is sent each frame
(define-record-type input (make-input time pose button-state) input?
  (time input-time)
  (pose input-pose)
  (button-state input-button-state))

;(: safe-ref (-> Any Nonnegative-Integer Any))
(define (safe-ref l r)
  (cond
    ((and (list? l) (> (length l) r)) (list-ref l r))
    (else #f)))
  
(define (input-from-sexp s) 
  (make-input (car s) (list->mat4 (car (cdr s))) (car (cdr (cdr s)))))
  
(define *input* (make-input 0.0 mat4-identity 0))
(define *input-prev* (make-input 0.0 mat4-identity 0))

; Since this is commonly used, it will be set automatically each frame.
; The pose matrix sent over with input each frame is a view matrix, the
; matrix that transforms world coordinates to eye coordinates.  The
; inverse of that would be the model matrix for a player head model with
; the origin at the eye center.
(define *pose-inverse* '())

(define (pressed-bit bit-index)
  (and (bitwise-bit-set? (input-button-state *input*) bit-index) (not (bitwise-bit-set? (input-button-state *input-prev*) bit-index))))

(define (held-bit bit-index)
  (bitwise-bit-set? (input-button-state *input*) bit-index))

(define (pressed-a)            (pressed-bit 0))
(define (pressed-b)            (pressed-bit 1))
(define (pressed-x)            (pressed-bit 2))
(define (pressed-y)            (pressed-bit 3))
(define (pressed-start)        (pressed-bit 4))
(define (pressed-back)         (pressed-bit 5))
(define (pressed-select)       (pressed-bit 6))
(define (pressed-menu)         (pressed-bit 7))
(define (pressed-right-trigger)(pressed-bit 8))
(define (pressed-left-trigger) (pressed-bit 9))
(define (pressed-dpad-up)      (pressed-bit 10))
(define (pressed-dpad-down)    (pressed-bit 11))
(define (pressed-dpad-left)    (pressed-bit 12))
(define (pressed-dpad-right)   (pressed-bit 13))
(define (pressed-lstick-up)    (pressed-bit 14))
(define (pressed-lstick-down)  (pressed-bit 15))
(define (pressed-lstick-left)  (pressed-bit 16))
(define (pressed-lstick-right) (pressed-bit 17))
(define (pressed-rstick-up)    (pressed-bit 18))
(define (pressed-rstick-down)  (pressed-bit 19))
(define (pressed-rstick-left)  (pressed-bit 20))
(define (pressed-rstick-right) (pressed-bit 21))
(define (pressed-touch)        (pressed-bit 22))
(define (pressed-swipe-up)     (pressed-bit 23))
(define (pressed-swipe-down)   (pressed-bit 24))
(define (pressed-swipe-left)   (pressed-bit 25))
(define (pressed-swipe-right)  (pressed-bit 26))

(define (held-a)            (held-bit 0))
(define (held-b)            (held-bit 1))
(define (held-x)            (held-bit 2))
(define (held-y)            (held-bit 3))
(define (held-start)        (held-bit 4))
(define (held-back)         (held-bit 5))
(define (held-select)       (held-bit 6))
(define (held-menu)         (held-bit 7))
(define (held-right-trigger)(held-bit 8))
(define (held-left-trigger) (held-bit 9))
(define (held-dpad-up)      (held-bit 10))
(define (held-dpad-down)    (held-bit 11))
(define (held-dpad-left)    (held-bit 12))
(define (held-dpad-right)   (held-bit 13))
(define (held-lstick-up)    (held-bit 14))
(define (held-lstick-down)  (held-bit 15))
(define (held-lstick-left)  (held-bit 16))
(define (held-lstick-right) (held-bit 17))
(define (held-rstick-up)    (held-bit 18))
(define (held-rstick-down)  (held-bit 19))
(define (held-rstick-left)  (held-bit 20))
(define (held-rstick-right) (held-bit 21))
(define (held-touch)        (held-bit 22))
(define (held-swipe-up)     (held-bit 23))
(define (held-swipe-down)   (held-bit 24))
(define (held-swipe-left)   (held-bit 25))
(define (held-swipe-right)  (held-bit 26))

; "action" can be either joypad A or touchpad tap
(define (pressed-action) (or (pressed-a) (pressed-touch)))

(define (held-action) (or (held-a) (held-touch)))

;--------------------------------------
; uri / init
;--------------------------------------
         
; This is mostly used by the uri macro, but it can be used to start
; a background download at any time.
(define (cmd-cache! uri)
  (cmd! (list 'cache uri)))

; If strings are defined with this macro, they will be automatically
; pre-cached on startup:
; (uri WAV-GAZE-ON     "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_gaze_on.wav")
(define-syntax uri
  (syntax-rules ()
    ((_ name address)
     (begin
       (cmd-cache! address)
       (define name address)))))

;--------------------------------------
; gaze tests
;--------------------------------------
; Test if a line intercepts a transformed bounds
(define (line-hit-bounds start end b)
  (car (intersect-ray-bounds start (vec3-normalize (sub3 end start)) (bounds3-min b) (bounds3-max b))))

(define (line-hit-transformed-bounds start end b xform)
;  (printf "line-hit-transformed-bounds: ~a ~a ~a ~a\n" start end b t)
  (define inv (mat4-inverse xform))
  (line-hit-bounds (mat4-trans3 inv start) (mat4-trans3 inv end) b))

(define (gaze-hit-transformed-bounds inp b xform)
  (line-hit-transformed-bounds (mat4-origin *pose-inverse*) (scale3 (mat4-forward *pose-inverse*) 1000.0) b xform))

#|
(intersect-ray-bounds (vec3 0.0 0.0 0.0) (vec3 0.0 0.0 -1.0) (vec3 -1.0 -1.0 -4.0) (vec3 1.0 1.0 -5.0))

(line-hit-transformed-bounds (vec3 0.0 1.430521 0.79084) 
                             (vec3 114.902 -520.203 -846.278) 
                             (bounds3 (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0)) 
                             (mat4 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 -2.0 0.0 0.0 0.0 1.0))
|#

;-----------------
; gaze-on-bounds
; Returns #t if the gaze is on the bounds.
;
; Tracks *gaze-on-this-frame* and  *gaze-close-this-frame* based on all
; calls made to this test function.
;
; gaze-effects
; Plays enter/leave sounds and enabled/disables the gaze cursor based on
; all the calls to ui-bounds this frame.
;-----------------
(define *gaze-close-this-frame* #f)
(define *gaze-on-this-frame* #f)
(define *gaze-on-last-frame* #f)

(define vec3-mid (make-vec3 0.5 0.5 0.5))

; Return the midpoint in world space of the transformed unit cube.
(define (center-of-transformed-unit-bounds trans)
  (mat4-trans3 trans vec3-mid))

(define (bent-gaze-forward center)
  (define start (mat4-origin *pose-inverse*))
  (define dir (mat4-forward *pose-inverse*))
  (define dir-center (vec3-normalize (sub3 center start)))
  (define dir-delta (sub3 dir-center dir))
  (define delta-len (vec3-length dir-delta))
;  (display (format "dir:~a dir-center:~a bent:~a\n" (vec3->string dir) (vec3->string dir-center) (vec3->string (add3 dir (scale3 dir-delta (min delta-len 0.1))))))
  (add3 dir (scale3 dir-delta (min 1.0 (/ 0.2 delta-len)))))

(define (gaze-is-close bounds-trans)
  (define forward (bent-gaze-forward (center-of-transformed-unit-bounds bounds-trans)))
  (line-hit-transformed-bounds (mat4-origin *pose-inverse*) (scale3 forward 1000.0) bounds3-unit bounds-trans))
  
(define *debug-gaze* #t)  ; if true, draw colored bounds for every gaze-on-bounds test

(define (gaze-on-bounds bounds xform)
  ; determine if we are gazing on it now
  (define gaze-on (gaze-hit-transformed-bounds *input* bounds3-unit xform))
  (define gaze-close (gaze-is-close xform))
  
  (set! *gaze-on-this-frame* (or *gaze-on-this-frame* gaze-on))
  (set! *gaze-close-this-frame* (or *gaze-close-this-frame* gaze-close))
                               
  ; For debugging, draw the collision bounds in different colors based
  ; on if the gaze is on it or close.
  (if *debug-gaze*
      (cmd-model! "_bounds" xform (cond
                                    (gaze-on   (opt-parm 1.0 0.0 0.0 1.0))
                                    ;  (gaze-close (opt-parm 1.0 1.0 0.0 1.0))
                                    (else       (opt-parm 0.0 1.0 0.0 1.0))))
      #f)
  
  ; return value of test
  gaze-on)

(uri WAV-GAZE-ON     "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_gaze_on.wav")
(uri WAV-GAZE-OFF    "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_gaze_off.wav")

(define (gaze-effects)
  ; if we are gazing on it now, and weren't last frame, play the on-sound
  (if (and *gaze-on-this-frame* (not *gaze-on-last-frame*))
    (cmd-local-sound! WAV-GAZE-ON)
    #f)
  
  ; if the last gaze-on was the previous frame, play the off-sound
  (if (and *gaze-on-last-frame* (not *gaze-on-this-frame*))
    (cmd-local-sound! WAV-GAZE-OFF)
    #f)

  ; enable the gaze cursor if gaze-on or gaze-close
  (cmd-gaze-cursor! 
   (if (or *gaze-on-this-frame* *gaze-close-this-frame*)
       1.4
       0.0))
  
  ; update state for next frame
  (set! *gaze-on-last-frame* *gaze-on-this-frame*)
  (set! *gaze-on-this-frame* #f)
  (set! *gaze-close-this-frame* #f)
  )


;--------------------------------------
; init-wrap
;
; An arbitrary sexpr can be passed to the script if it was launched from
; another script.
;
; The init function returns the command-list that has been built
; up by the uri macros and other functions before any frames have
; been executed.
;--------------------------------------

; Boilerplate called either by remote.rkt or directly form NetHmd with Chibi.

(define init-parms '())

; Arbitrary key/value information can be passed to the init function.
; Defined symbols:
; "ip"      : ip address of phone
; "userid"  : User id from the Oculus platform (TODO)
;
(define (init-parm key)
  (define pair (assoc key init-parms))
  (if pair (cadr pair) #f))
  
(define (init-wrap init-function init-sexp)
  (set! init-parms init-sexp)
(display (format "init parms: ~s\n" init-parms)) 
  (if init-function
      (init-function init-sexp)
      #f)
  *frame-commands*)

;--------------------------------------
; tic-wrap
;--------------------------------------
  
(define *frame-number* 0)

; Boilerplate called either by remote.rkt or directly form NetHmd with Chibi.
(define (tic-wrap tic-function input-sexp)
  (if (not (list? input-sexp))
      (error "Bad input.")
      #f)
        
;  (display (format "input: ~a\n" input-sexp))
  
  (set! *input-prev* *input*)  
  (set! *input* (input-from-sexp input-sexp))
  (set! *pose-inverse* (mat4-inverse (input-pose *input*)))
  
  (set! *frame-number* (+ 1 *frame-number*))
  
  ; clear commands
  (set! *frame-commands* '())
  
  ; Run the frame to generate commands
  ; and update the world state.
  (tic-function)

  ; Play sounds and enable the gaze cursor based on tests for active elements
  (gaze-effects)
  
  ; Dump for debugging
;  (write *frame-commands*)
  *frame-commands*)

(display "parsed file\n")
