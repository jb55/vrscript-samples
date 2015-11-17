#lang racket/base
; TODO: change things to be more r7rs like?

; Everything here is exported
(provide (all-defined-out))
(require srfi/9)  ; define-record-type

;(require rnrs/base-6)

; everything up to this mark will be stripped and replaced when embedded
; to work around compatibility issues 
; %%%END-OF-HEADER%%%

;------------------------------------------------------------------

(define pi 3.141592657)
(define pi/2 (/ 3.141592657 2.0))
(define pi*2 (* 3.141592657 2.0))
(define pi*3/2 (* 3.0 (/ 3.141592657 2.0)))
(define -pi/2 (- 0.0 (/ 3.141592657 2.0)))
(define -pi*2 (- 0.0 (* 3.141592657 2.0)))
(define -pi*3/2 (- 0.0 (* 3.0 (/ 3.141592657 2.0))))
(define pi/180 (/ pi 180.0))
(define pi/180-inv (/ 180.0 pi))
  
(define (degrees->radians d) (* d pi/180))
(define (radians->degrees r) (* r pi/180-inv))

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

; http://inside.mines.edu/fs_home/gmurray/ArbitraryAxisRotation/
(define (mat4-rotate-around unit-vector radians)
  (define c (cos radians))
  (define s (sin radians))
  (define mc (- 1.0 c))
  (define u (vec3-x unit-vector))
  (define v (vec3-y unit-vector))
  (define w (vec3-z unit-vector))
  (define u2 (* u u))
  (define v2 (* v v))
  (define w2 (* w w))
  (make-mat4 (+ u2 (* (- 1.0 u2) c))     (- (* u v mc) (* w s))     (+ (* u w mc) (* v s))     0.0
             (+ (* u v mc) (* w s))      (+ v2 (* (- 1.0 v2) c))    (- (* v w mc) (* u s))     0.0
             (- (* u w mc) (* v s))      (+ (* v w mc) (* u s))     (+ w2 (* (- 1.0 w2) c))    0.0
             0.0                         0.0                        0.0                        1.0))
  

; Transform a vector by a matrix

; A direction is just transformed by the inner 3x3, ignoring translation.  Should we still divide by w?
(define (mat4-transform-dir m v) (make-vec3 (+ (* (mat4-m00 m) (vec3-x v)) (* (mat4-m01 m) (vec3-y v)) (* (mat4-m02 m) (vec3-z v)))
                                                     (+ (* (mat4-m10 m) (vec3-x v)) (* (mat4-m11 m) (vec3-y v)) (* (mat4-m12 m) (vec3-z v)))
                                                     (+ (* (mat4-m20 m) (vec3-x v)) (* (mat4-m21 m) (vec3-y v)) (* (mat4-m22 m) (vec3-z v)))))
(define (mat4-transform3 m v) (make-vec3 (+ (* (mat4-m00 m) (vec3-x v)) (* (mat4-m01 m) (vec3-y v)) (* (mat4-m02 m) (vec3-z v)) (mat4-m03 m))
                                                  (+ (* (mat4-m10 m) (vec3-x v)) (* (mat4-m11 m) (vec3-y v)) (* (mat4-m12 m) (vec3-z v)) (mat4-m13 m))
                                                  (+ (* (mat4-m20 m) (vec3-x v)) (* (mat4-m21 m) (vec3-y v)) (* (mat4-m22 m) (vec3-z v)) (mat4-m23 m))))
(define (mat4-transform4 m v) (make-vec4 (+ (* (mat4-m00 m) (vec4-x v)) (* (mat4-m01 m) (vec4-y v)) (* (mat4-m02 m) (vec4-z v)) (* (mat4-m03 m) (vec4-w v)))
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

(define (cross a b) (make-vec3 (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
                                        (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
                                        (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b)))))
 
(define (dot3 a b) (+ (* (vec3-x a) (vec3-x b)) (* (vec3-y a) (vec3-y b)) (* (vec3-z a) (vec3-z b))))
(define (add3 a b) (make-vec3 (+ (vec3-x a) (vec3-x b)) (+ (vec3-y a) (vec3-y b)) (+ (vec3-z a) (vec3-z b))))
(define (sub3 a b) (make-vec3 (- (vec3-x a) (vec3-x b)) (- (vec3-y a) (vec3-y b)) (- (vec3-z a) (vec3-z b))))
(define (mul3 a b) (make-vec3 (* (vec3-x a) (vec3-x b)) (* (vec3-y a) (vec3-y b)) (* (vec3-z a) (vec3-z b))))
(define (neg3 a) (make-vec3 (- 0.0 (vec3-x a)) (- 0.0 (vec3-y a)) (- 0.0 (vec3-z a))))
(define (scale3 a s) (make-vec3 (* (vec3-x a) s) (* (vec3-y a) s) (* (vec3-z a) s)))

(define (vec3-length v) (sqrt (dot3 v v)))
(define (vec3-normalize v) (scale3 v (/ 1.0 (vec3-length v))))
(define (vec3->string v) (format "(~a ~a ~a)" (vec3-x v) (vec3-y v) (vec3-z v)))

(define (bounds3-midpoint b) (scale3 (add3 (bounds3-min b) (bounds3-max b)) 0.5))

; Extract information from a mat4 model matrix
(define (mat4-origin m) (make-vec3 (mat4-m03 m) (mat4-m13 m) (mat4-m23 m)))
(define (mat4-forward m) (mat4-transform-dir m (make-vec3 0.0 0.0 -1.0)))
(define (mat4-back m) (mat4-transform-dir m (make-vec3 0.0 0.0 1.0)))
(define (mat4-left m) (mat4-transform-dir m (make-vec3 -1.0 0.0 0.0)))
(define (mat4-right m) (mat4-transform-dir m (make-vec3 1.0 0.0 0.0)))
(define (mat4-up m) (mat4-transform-dir m (make-vec3 0.0 1.0 0.0)))
(define (mat4-down m) (mat4-transform-dir m (make-vec3 0.0 -1.0 0.0)))

;-------------------------------------------------------
; Ray / line tracing
;-------------------------------------------------------

; Straight port from ModelTrace.cpp
; returns true if the ray starting from start and extending
; infinitely in dir hits the bounds.
; 
(define (ray-hits-min-max? start dir mins maxs)
  (define rcpDir (make-vec3 (safe-rcp (vec3-x dir)) (safe-rcp (vec3-y dir)) (safe-rcp (vec3-z dir))))
  (define s (mul3 (sub3 mins start) rcpDir))
  (define t (mul3 (sub3 maxs start) rcpDir))
  (define minHit (make-vec3 (min (vec3-x s) (vec3-x t)) (min (vec3-y s) (vec3-y t)) (min (vec3-z s) (vec3-z t))))
  (define maxHit (make-vec3 (max (vec3-x s) (vec3-x t)) (max (vec3-y s) (vec3-y t)) (max (vec3-z s) (vec3-z t))))
  (define t0 (max (vec3-x minHit) (vec3-y minHit) (vec3-z minHit)))
  (define t1 (min (vec3-x maxHit) (vec3-y maxHit) (vec3-z maxHit)))
  (and (> t0 0.0) (<= t0 t1)))

; Test if a ray intercepts a transformed bounds
(define (ray-hits-bounds? start dir b)
  (ray-hits-min-max? start (vec3-normalize dir) (bounds3-min b) (bounds3-max b)))

(define (ray-hits-transformed-bounds? start dir b xform)
  (define inv (mat4-inverse xform)) ; FIXME: slow!
  (ray-hits-bounds? (mat4-transform3 inv start) (mat4-transform-dir inv dir) b))


; Returns the xy0 intersection of a line on the z=0 plane of xform.
; Dir does not need to be normalized.
; The line is infinite in both directions.
; Very large, but not crashing, values will be returned
; for a parallel trace that would not, in theory, intersect.
(define (intersect-line-plane start dir xform)
  (define inv (mat4-inverse xform))
  (define lstart (mat4-transform3 inv start))
  (define ldir (mat4-transform-dir inv dir))
  (define spd (safe-rcp (vec3-z ldir)))
  (define pt (sub3 lstart (scale3 ldir (* (vec3-z lstart) spd))))
  (make-vec3 (vec3-x pt) (vec3-y pt) 0.0))

; 1-y flips the y coordinate of intersect-plane for when you want
; a texture coordinate instead of a position, since our default
; geometry flips the y texcoord like almost all engines do.
(define (intersect-line-texture start dir xform)
  (define pt (intersect-line-plane start dir xform))
  (make-vec3 (vec3-x pt) (- 1.0 (vec3-y pt)) 0.0))


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
; Misc scheme helpers
;-------------------------------------------------------

; Having problems importing SRFI 1
; iota returns a list from 0 (inclusive) to stop (exclusive)
(define (iota stop)
  (define (rev x)
    (if (= x stop)
        '()
        (cons x (rev (+ x 1)))))
  (rev 0))


(define (floor->exact x) (inexact->exact (floor x)))

; Return a new list with one element replaced.
; TODO: get vectors working for tic-parms, need to fix some stuff in the C++ Sexp.
(define (list-replace lst x v)
  (cond
    ((= x 0) (cons v (cdr lst)))
    (#t (cons (car lst) (list-replace (cdr lst) (- x 1) v)))))

;(: safe-ref (-> Any Nonnegative-Integer Any))
(define (safe-ref l r)
  (cond
    ((and (list? l) (> (length l) r)) (list-ref l r))
    (else #f)))
  
; The *client-state* and *server-state* s-expressions need to be
; turned into a single string that is passed through the network
; system without being broken down into constituent s-expressions.
; This allows any data type that is handled by read and write to
; be communicated without my C++ sexpr parsing needing to understand it.
(define (sexpr->string s)
  (define p (open-output-string))
  (write s p)
  (get-output-string p))

(define (string->sexpr s)
  (define p (open-input-string s))
  (read p))

; https://en.wikipedia.org/wiki/Linear_congruential_generator
(define *rand-seed* 1234)
(define rand-max 32767)

; (0 to rand-max)
(define (rand!)
  ; This is going to exceed the fixnum range in 32 bit scheme as
  ; an intermediate if nothing else.  If I turn off bignum support
  ; in Chibi it will have to be revisited.
  (set! *rand-seed* (bitwise-and #xffffffff (+ 12345 (* 1103515245 *rand-seed*))))
  (bitwise-and rand-max (arithmetic-shift *rand-seed* -16)))

; (0 to max)
(define (random-int! max)
  (remainder (rand!) max))

; (0.0 to 1.0]
(define (random-float!)
  (* (rand!) 0.000030517578125))

; [-1.0 to 1.0]
(define (random-float-c!)
  (- (* (random-float!) 2.0) 1.0))

;------------------------------------------------------------
; social commands
;
; Names are unique across accounts, but a single account can
; be logged into multiple devices, so per-session fixnum
; client-id are required to differentiate them.  Client-id
; for a real client will never be 0, so that is a safe "no-client"
; value.
;
; The system automatically adds and removes client-id from the
; *client-seats* list, zeroing out clients that leave and placing
; new clients in the first zero slot.  It is guaranteed that when
; frame is called, *local-client-id* will be present in *client-seats*.
; The controlling client is free to rearrange the seating whenever
; desired.
;
; There will be brief periods when a client-id is present in the
; *client-seats* list, but not in the *clients* list, meaning it has
; been assigned a spot, but we haven't received a state packet from
; it yet, so we don't know the client-state.
;
; These variables are not valid during (init)
;------------------------------------------------------------

; This is necessary to distinguish between the case of a single-player
; experience and a multi-player experience with only only player currently
; connected.
;
; Set at init and never dynamically modified.
(define social? #f)

(define *local-client-id* 0)  ; this is never going to change, should we remove *earmuffs*?
(define *controlling-client-id* 0) ; The client-id that controls the server-state and client-seats
(define *server-state* '())
(define *client-state* '())   ; copy of (client-state (client 0))

(define *client-seats* '(0 0 0 0 0 0 0 0))

(define (seat-set! seat cid)
  (set! *client-seats* (list-replace *client-seats* seat cid)))

(define (seat-get seat)
  (list-ref *client-seats* seat))

; Swap-seats is preferred to seat-set!, because it can't break the invariant
; on *client-seats*.
(define (swap-seats! a b)
  (define tmp (seat-get a))
  (seat-set! a (seat-get b))
  (seat-set! b tmp))
  
; This list will always include the local client in the first position.
(define *clients* '())

; Convenience function
(define (client-index x)
  (list-ref *clients* x))

; It is an error to try to set server-state if you aren't the controlling-client.
; The packet server sets the controlling-client as the client that has been on the
; server longest.
(define (controlling-client?)
  (= *controlling-client-id* *local-client-id* ))

; Racket doesn't allow you to directly set! a variable defined in another
; module, so these setter function are provided.  They will both typically
; be vectors, which can have indiviual elements referenced and changed
; directly by scripts after they have been defined.
(define (set-server-state! state)
  (set! *server-state* state))

(define (set-client-state! state)
  (set! *client-state* state))

; Each frame will update all the players.
; TODO: add model index and any other client state
(define-record-type client
  (make-client name id pose state)
  client?
  (name client-name)    ; string
  (id client-id)        ; integer user id
  (pose client-pose)    ; mat4
  (state client-state)) ; arbitrary s-expression

; Returns a client, or #f if not found.
(define (client-by-id id)
  (define (scan lst)
    (cond
      ((null? lst) #f)
      ((= id (client-id (car lst))) (car lst))
      (#t (scan (cdr lst)))))
  (scan *clients*))


;-------------------------------------------------------
; sys commands write to the communications channel
;
; The convention is to name functions that add commands to
; the *frame-commands* list with a leading +.  While they
; do mutate global state, no other script code (should) looks
; at the state until it is returned to the host, so giving
; them the conventional ! suffic for a mutating function is
; unnecessary.
;-------------------------------------------------------

; A list of s-expressions that will be compressed and sent to the HMD.
(define *frame-commands* '())

(define (+cmd s) 
  (set! *frame-commands* (cons s *frame-commands*)))

; Finish will cause a fade to black and script exit as if the user
; had pressed the back button, popping back to the launching
; script or out to the launching application.
(define (+finish)
  (+cmd '(finish)))

; The master server will be informed to start advertizing this
; script and acting as a packet server for it, and the script will
; be restarted in social mode with the current state.
(define (+share-server max-clients public title icon-uri)
  (if (controlling-client?)
      (begin
        (display "Server shared!\n")        
        (+cmd (list 'share-server max-clients public title icon-uri)))
      #f))

; Load and run a new script, optionally passing an s-expression to the new init function.
(define (+link uri . parms) (+cmd (append (list 'link uri) parms)))

; Abort the current script and display an error message
(define (+error str) (+cmd (list 'error str)))

;-------------------------------------------------------
; user positioning
;
; If it is not explicitly set, the eye position when starting a script
; will be at (0 0 0) in standard OpenGL orientation -- looking down
; -Z, with +X to the right and +Y up.  Users can turn around to
; any angle, but reset orientation will return here.
;
; Note that the head-neck model and HMD position tracking will move
; the exact view position somewhat away from the programmed foot
; position.
;
; Note that currently, free joypad movement is allowed if the position
; isn't reset every frame.  This will probably be turned into an option.
;
; Move the local client to a new position and default orientation.
; This is the "neautral eye position", the foot position used by the host
; VrScene code for collision detection is derived from this, and the
; origin extracted from the input pose will move around somewhat based on the
; head/neck model.
;
; 0 yaw is default looking down -Z, pi/2 is
; lookingdown -X, pi is down +Z, pi*3/2 is down +X.
;
; No interface is given for adjusting reference pitch or roll
; angles, because the gravity vector should be sacrosanct in VR!
;-------------------------------------------------------

(define (+set-position pos yaw-radians)
  (+cmd (list 'pos (vec3->list pos) yaw-radians)))
; TODO: velocity for extrapolation on dropped frames once I get the scripts
; running in another thread properly.

; Reorient will make the user's current direction yaw 0.0.
; If an environment lets them look in all directions, but you want
; each transition to look at a particular thing, reorient when
; doing the set-position.
(define (+reorient)
  (+cmd (list 'reorient)))
  
;------------------------------------------------------------
; video commands
;
; The video will appear as an external image on texture slot 0.
;------------------------------------------------------------

(define (+video uri . opts)
  (+cmd (append (list 'video uri) opts)))


;------------------------------------------------------------
; audio commands
;
; TODO: volume and frequency control.
; TODO: precise timing for updates as well as starts?
; Note that stereo wav files cannot be positioned, they will
; always play the same to the left and right ears regardless
; of head orientation or specified position.
;------------------------------------------------------------

; 'loop is a valid atomic option flag.
(define (+sound uri . opts)
  (+cmd (append (list 'sound (list 'wav uri)) opts)))

(define (+sound-update name . opts)
  (+cmd (append (list 'sound (list 'name name)) opts)))

; A sound without a position will play at full volume in both ears
; without 3D spatialization.  Stereo wav files are always played
; without spatialization.
(define (opt-position v)
  (list 'position (vec3->list v)))

; A static sound with a position will automatically update as the
; user looks around, but for a sound to travel with a moving object
; it must be given a name and have opt-position updated with subsequent
; additional cmd-sound! commands.
; Name should be an integer.
(define (opt-name name)
  (list 'name name))

; If sounds need to be precisely timed, they can be started at an
; exact time in the future, which will be sample-accurate.  If the
; time is in the past, the start of the sound will be chopped off.
; TODO: buffer multiple sounds with the same name and different times.
(define (opt-time time)
  (list 'time time))

(define (opt-volume volume)
  (list 'volume volume))
 
  
; The only way to stop sounds without a name is to stop all sounds.
(define (+stop-sounds)
  (+cmd (list 'stop-sounds)))

; experimental: option parms
(define (+delimited-sound . args)
  (define wav #f)
  (define position #f)
  (define name 0)
  (define volume 1.0)
  (define looping #f)
  (define (parse args)
    (cond
      ((null? args) #f)
      ((eq? (car args) 'wav) (set! wav (cadr args))
                             (parse (cddr args)))
      ((eq? (car args) 'position) (set! position (vec3->list (cadr args)))
                                  (parse (cddr args)))
      ((eq? (car args) 'name) (set! name (cadr args))
                              (parse (cddr args)))
      ((eq? (car args) 'volume) (set! volume (cadr args))
                                (parse (cddr args)))
      ((eq? (car args) 'looping) (set! looping #t)
                                (parse (cdr args)))
      (else (+error (format "cmd-sound! unknown option: ~a\n" (car args))))))
  (if (null? args)
      (+error "cmd-sound! with no parameters")
      (begin
        (if (string? (car args))
            (begin
              (set! wav (car args))
              (parse (cdr args)))
            (parse args))
        (+cmd 'sound wav position name volume looping))))
                           
;------------------------------------------------------------
; model commands
;------------------------------------------------------------

(define TEXT_HORIZONTAL_LEFT 0)
(define TEXT_HORIZONTAL_CENTER 1)
(define TEXT_HORIZONTAL_RIGHT 2)

(define TEXT_VERTICAL_BASELINE 0)      ; align text by baseline of first row
(define TEXT_VERTICAL_CENTER 1)
(define TEXT_VERTICAL_CENTER_FIXEDHEIGHT 2) ; ignores ascenders/descenders
(define TEXT_VERTICAL_TOP 3)

; Specify size as font height?
(define (+text txt xform . options) (+cmd (append (list 'text txt TEXT_HORIZONTAL_CENTER TEXT_VERTICAL_BASELINE (mat4->list xform)) options)))
(define (+text-ext txt horiz vert xform . options) (+cmd (append (list 'text txt horiz vert (mat4->list xform)) options)))

; Creates a textured quad that goes from 0.0 0.0 0.0 to 1.0 1.0 0.0 with texture
; coordinates that go from 0.0 0.0 to 1.0 1.0.
; The front face of the quad is the +Z side, so translating it by 0.0 0.0 -1.0 will
; make it occupy the upper right quarter of a 90 degree view from the origin with the
; expected orientation.
(define (+quad uri xform . options) (+cmd (append (list 'quad uri (mat4->list xform)) options)))

; The previous frame will be faded to black before this frame is applied and faded in.
; TODO: test this still works
(define (+fade) (+cmd (list 'fade)))

; Only yaw orientations make sense for most panos.
; Changing pano will imply a fade out / fade in.  It may stay
; faded to black for a significant amount of time if the pano is large and the
; net is slow.
(define (+pano uri) (+cmd (list 'pano uri)))

(define (+clear r g b a) (+cmd (list 'clear r g b a)))

(define (+model uri xform . options) (+cmd (append (list 'model uri (mat4->list xform)) options)))

; Shaders can be defined globally or during init, then referenced with a shader parm on a surface.
; It is not legal to define shaders later, because clients joining after that point would
; never execute that command.
(define (+shader name vertex fragment) (if init-completed
                                           (+error (format "Shader ~a defined after init" name))
                                           (+cmd (list 'shader name vertex fragment))))

; A 0.0 distance disables the gaze cursor.
(define (+gaze-cursor distance) (+cmd (list 'gaze-cursor distance)))

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

; Texture override
; Allow any number of textures to be set (limited in vrscript to MAX_PROGRAM_TEXTURES)
(define (opt-texture tx . extra) (append (list 'texture tx) extra))

;--------------------------------------
; development output
;
; TODO: have app capture all log output and allow a console toggle.  That should
; be handled completely outside script.
;--------------------------------------

; Displays text in a juddering face-locked HUD.  Don't use this for real applications!
; This is for continuously updated data, it vanishes the following frame.
; TODO: consider adding support for a TimeWarp HUD that would never judder, like Stratum.
(define (+hud text)
  (+text text (mat4-compose (mat4-translate 0.0 0.5 -1.0) *pose-inverse*)))

;--------------------------------------
; time
;
; *script-seconds* is a biased real-time value that is monotonically
; increasing with no per-frame clamping.  It will not change during tic
; processing, so it cannot be used for profiling internal functions, but
; you can tell if there was an unusually large gap between tics.
;
; In a non-social game, *script-seconds* will start at 0.0 for the first (tic).
;
; In a social game, all clients see the same *script-seconds*, which starts at
; 0.0 when the server was first created on the master server, which will usually be
; a second or two before the initial client runs a (tic).  A server that doesn't
; halt-on-empty may be up for weeks at a time, so the second count can get quite large,
; but flonums in scheme are doubles, so there is no danger of precision loss.
;
; Sounds and videos can (TODO) be explicitly referenced to script-seconds, instead of
; the implicit "now", but explicit time references are invalid during (init),
; before the time base has been set.
;
; Internally there are three time bases: local time, server time, and script time,
; but it is a bug if the system doesn't completely hide it from script.
;
; To be precise, the local time that script time is corrected to is the
; PredictedDisplayTimeInSeconds from VrFrame, which is the midpoint of the expected
; time that the frame generated by this (tic) will be scanned out to video.  If you
; are holding frame rate, that will be when the raster is in the middle of the screen.
;
;--------------------------------------

; Updated from input-time by (tic)
(define *script-seconds* 0.0)

; *script-seconds* from previous (tic)
; will me MAX_FLT on the first tic, so no time triggers will fire
; when a client joins a server that has been running for some time.
(define *prev-script-seconds* 3.402823466385288599e+38)

(define (crossed-seconds s)
  (and (>= *script-seconds* s) (< *prev-script-seconds* s)))
  
;--------------------------------------
; input
;--------------------------------------
 
; The input structure is sent each frame
; Time will be synchronized for all social clients
(define-record-type input (make-input time pose button-state) input?
  (time input-time)
  (pose input-pose)
  (button-state input-button-state))

(define (input-from-sexp s) 
  (make-input (car s) (list->mat4 (car (cdr s))) (car (cdr (cdr s)))))

; It is often convenient to be able to refer to the previous frame's
; input to check for transitions.
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
(define (pressed-swipe-forward) (pressed-bit 25))
(define (pressed-swipe-back)   (pressed-bit 26))

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
(define (held-swipe-forward) (held-bit 25))
(define (held-swipe-back)   (held-bit 26))

; "action" can be either joypad A or touchpad tap
(define (pressed-action) (or (pressed-a) (pressed-touch)))

(define (held-action) (or (held-a) (held-touch)))

;--------------------------------------
; uri / init
;--------------------------------------
         
; This is mostly used by the uri macro, but it can be used to start
; a background download at any time.
(define (+cache uri)
  (+cmd (list 'cache uri)))

; If strings are defined with this macro, they will be automatically
; pre-cached on startup:
; (uri WAV-GAZE-ON     "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_gaze_on.wav")
(define-syntax uri
  (syntax-rules ()
    ((_ name address)
     (begin
       (+cache address)
       (define name address)))))

;-----------------
; gaze-on-bounds?
;
; Returns #t if the gaze is on the transformed bounds.
; Gaze is a ray defined by the current pose, extending forward to infinity.
;
; Tracks *gaze-on-this-frame* and  *gaze-close-this-frame* based on all
; calls made to this test function.
;
; gaze-effects
; Plays enter/leave sounds and enabled/disables the gaze cursor based on
; all the calls to ui-bounds this frame.
;-----------------
(define *gaze-close-this-frame* #f)
(define *gaze-close-last-frame* #f)
(define *gaze-on-this-frame* #f)
(define *gaze-on-last-frame* #f)
(define *debug-gaze* #f)  ; if true, draw colored bounds for every gaze-on-bounds test

; Return the midpoint in world space of the transformed bounds
(define (center-of-transformed-bounds bounds trans)
  (mat4-transform3 trans (bounds3-midpoint bounds)))

(define *gaze-close-this-test* #f) ; hacky global for second return from gaze-on-bounds?

(define (gaze-on-bounds? bounds xform)
  (define start (mat4-origin *pose-inverse*))
  (define (bent-gaze-forward center)
    (define dir (mat4-forward *pose-inverse*))
    (define dir-center (vec3-normalize (sub3 center start)))
    (define dir-delta (sub3 dir-center dir))
    (define delta-len (vec3-length dir-delta))
    (add3 dir (scale3 dir-delta (min 1.0 (/ 0.1 delta-len)))))
  
  ; Do the close test first, and skip the on test if not close
  (define forward (bent-gaze-forward (center-of-transformed-bounds bounds xform)))
  (set! *gaze-close-this-test* (ray-hits-transformed-bounds? start
                                                   forward
                                                   bounds
                                                   xform))
  
  ; Check for exactly hitting it
  (define gaze-on (and *gaze-close-this-test*
                       (ray-hits-transformed-bounds? start
                                                     (mat4-forward *pose-inverse*)
                                                     bounds
                                                     xform)))
  
  (set! *gaze-on-this-frame* (or *gaze-on-this-frame* gaze-on))
  (set! *gaze-close-this-frame* (or *gaze-close-this-frame* *gaze-close-this-test*))
                               
  ; For debugging, draw the collision bounds in different colors based
  ; on if the gaze is on it or close.
  (if *debug-gaze*
      (+model "_bounds" xform (cond
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
    (+sound WAV-GAZE-ON)
    #f)
  
  ; if the last gaze-on was the previous frame, play the off-sound
  (if (and *gaze-on-last-frame* (not *gaze-on-this-frame*))
    (+sound WAV-GAZE-OFF)
    #f)

  ; enable the gaze cursor if gaze-on or gaze-close
  (+gaze-cursor
   (if (or *gaze-on-this-frame* *gaze-close-this-frame*)
       1.4
       0.0))
  
  ; update state for next frame
  (set! *gaze-on-last-frame* *gaze-on-this-frame*)
  (set! *gaze-close-last-frame* *gaze-close-this-frame*)
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
;
; server-state must be initialized and returned, but social games
; will have it immediately replaced with the data from the packet server.
;--------------------------------------

; Set this true to get dumps of input and output for (init) and the first (frame).
; It can be rather bulky.
(define debug-messages #f) 


; Boilerplate called either by remote.rkt or directly form vrscript with Chibi.

; Some commands, like shader and geometry specification, are only
; legal in global definitions or during init, because they must
; always be available to clients that join later.
(define init-completed #f)

(define init-parms '())

; Arbitrary key/value information can be passed to the init function.
; Defined symbols:
; "ip"      : ip address of phone
; "social"  : #t if running networked
;
(define (init-parm key)
  (define pair (assoc key init-parms))
  (if pair (cadr pair) #f))
  
(define (init-wrap init-function init-sexp)
  (when debug-messages
    (printf "init parms: ~s\n" init-sexp))
  
  ; Pull some information out of init-sexp now, but
  ; the full list is retained for later reference.
  (set! init-parms init-sexp)
  (set! social? (init-parm "social"))

  ; Run the init function if it exists.
  (when init-function
      (init-function init-sexp))
  
  ; Always send the server state and client state
  (+cmd (list 'client-state (sexpr->string *client-state*)))
  (+cmd (list 'server-state (list *controlling-client-id* *client-seats* (sexpr->string *server-state*))))

  ; Reverse the list so it is executed in the order it was created
  (set! *frame-commands* (reverse *frame-commands*))
  
  ; Dump for debugging.
  (when debug-messages
    (printf "Init return: ~s\n" *frame-commands*))

  ; Shader and geometry generation is now illegal.
  (set! init-completed #t)
  
  *frame-commands*)

;--------------------------------------
; tic-wrap
;
; ( <input-struct> (local-client-id (controlling-client-id (client-seats) server-state-string)) ( <client-state-string> ... ) ) )
; Returns the *frame-commands* list.
;--------------------------------------
  
(define *frame-number* 0)
(define *tic-input* '())

; Boilerplate called either by remote.rkt or directly form NetHmd with Chibi.
(define (tic-wrap tic-function tic-input)
  (when (and debug-messages (= 1 *frame-number*))
     (printf "Initial ~a tic parms: ~s\n" (length tic-input) tic-input))
  
  (set! *tic-input* tic-input)

  ; pull out the multi-player 
  (define multi (list-ref tic-input 1))

  (set! *local-client-id* (list-ref multi 0))

  ; pull out the MutableServerState
  (define mss (list-ref multi 1))
  
  (set! *controlling-client-id* (list-ref mss 0))
  (set! *client-seats* (list-ref mss 1))

  ; If in social mode, *server-state* may be modified by other clients
  ; if we aren't the controlling client.
  (when social?
    (set! *server-state* (string->sexpr (list-ref mss 2))))
    
  ; Turn each client list into a client record and convert the
  ; opaque application specific state string into an s-expression.
  (set! *clients* (map (lambda (c)
                         (make-client (list-ref c 0) (list-ref c 1) (list-ref c 2) (string->sexpr (list-ref c 3))))
                         (list-ref multi 2)))
  
  (set! *input-prev* *input*)  
  (set! *input* (input-from-sexp (list-ref tic-input 0)))
  (set! *pose-inverse* (mat4-inverse (input-pose *input*)))
  (set! *frame-number* (+ 1 *frame-number*))
  (set! *script-seconds* (input-time *input*))
  
  ; clear commands
  (set! *frame-commands* '())

  ; Run the frame to generate commands
  ; and update global state.
  (tic-function)

  ; Play sounds and enable the gaze cursor based on tests for active elements
  (gaze-effects)

  ; Move over *prev-script-seconds* for the next frame
  (set! *prev-script-seconds* *script-seconds*)
  
  ; Always add the client-state and server-state messages
  (+cmd (list 'client-state (sexpr->string *client-state*)))
  (+cmd (list 'server-state (list *controlling-client-id* *client-seats* (sexpr->string *server-state*))))

  ; Reverse the list so it is executed in the order it was created
  (set! *frame-commands* (reverse *frame-commands*))
    
  ; Dump for debugging.
  (when (and debug-messages (= 1 *frame-number*))
     (printf "Initial tic return: ~s\n" *frame-commands*))

  ; Return the command list to the host program.
  *frame-commands*)

; C accelerator functions
;(define mt1 (mat4-rotate-x 1.0))
;(define mt2 (mat4-rotate-y 1.0))
;(printf "scheme: ~s\n" (mat4->list (mat4-mul mt1 mt2)))
;(printf "ffi: ~s\n" (mat4->list (mat4-mul-c mt1 mt2)))

