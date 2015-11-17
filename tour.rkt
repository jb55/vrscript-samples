#lang racket/base
(require "remote.rkt")

; local mutable state for animations and transitions from server states
(define *swipe-anim-dir* 0.0)      ; yaw degrees of view to place the swipe anim
(define *current-scene* -1)        ; determine that the server has transitioned
(define *current-destination* -1)

; When the scene transitions, all the uri needed for the new scene will be fetched.
(define *fetch-gather* #f)
(define *fetch-uri* '())
                             
(define *transition-time* -100.0)   ; time when *current-scene* finished fetching data

;-----------------------------------------------------
; Social
;
; Server state:
; #( destination scene cmd-num pic-visible )
;
(define (ss-destination) (vector-ref *server-state* 0))
(define (ss-scene)       (vector-ref *server-state* 1))
(define (ss-cmd-num)     (vector-ref *server-state* 2))
(define (ss-pic-visible) (vector-ref *server-state* 3))

(define (ss-destination! x) (vector-set! *server-state* 0 x))
(define (ss-scene! x)       (vector-set! *server-state* 1 x))
(define (ss-cmd-num! x)     (vector-set! *server-state* 2 x))
(define (ss-pic-visible! x)  (vector-set! *server-state* 3 x))


; initial server state:
(set-server-state! (vector 0 0 0 ""))

; The server will execute the client commands, changing server-state.
(define (client-command! cmd parm)
  (set-client-state! (vector (+ 1 (ss-cmd-num)) cmd parm)))

; initial client state:
(set-client-state! (vector 0 1 #f #f))


; To allow social parameters to be set up the same way for both user-invoked and
; remote development invoked, the parameters are defined here, and passed in
; both places.
; ( max-players shutdown-on-empty title icon )
(define server-parms '(3 #t "VR Tour" "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/tour.png"))

; Data that will be pre-cached before the first frame is rendered.
; The uri macro defines the name and adds a cache command to the init command list.
(uri flightboard_main "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/Models/flightboard_main.ovrscene")
(uri WAV-ACTIVATE    "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_activate_01.wav")


;-----------------
; departure-button
;
; Tha airport destination buttons in two columns
;-----------------
(define (departure-button title slot)
  (define xtrans (if (<= slot 5) -1.1 0.11))
  (define ytrans (- 0.8 (* (remainder slot 6) 0.14)))
  (define bounds-trans (mat4-compose (mat4-translate 0.0 -0.3 -0.5) 
                                     (mat4-scale/xyz 1.0 0.12 0.01) 
                                     (mat4-translate xtrans ytrans -2.0)))
  (define gaze-now (gaze-on-bounds? bounds3-unit bounds-trans))
  
  ; Position the text
  (+text-ext title TEXT_HORIZONTAL_LEFT TEXT_VERTICAL_BASELINE
             (mat4-compose 
              (mat4-scale 1.8) 
              (mat4-translate xtrans ytrans -2.0))
             (if gaze-now 
                 (opt-parm 1.0 1.0 0.5 1.0) 
                 (opt-parm 1.0 1.0 1.0 1.0)))
  
  ; if an input click just happened and we are gazing on it, return true
  (if (and (pressed-action) gaze-now)
      (begin
        (+sound WAV-ACTIVATE)
        #t)
      #f)
  )

(define (departure-button-list lst)
  (for-each (lambda (index)
              (define db (list-ref lst index))
              (define title (list-ref db 0))
              (define pic-list (list-ref db 1))
              (when (departure-button title index)
                (client-command! 'destination (+ 1 index))))
            (iota (length lst))))
  
(define (share-button)
  (if social?
      (departure-button "Open for social!" 11)
      (when (departure-button "Share Server" 11)
        (apply +share-server server-parms))))

;----------------------
; set-position
;
; Seat all the clients relative to each other.
;----------------------
(define (set-position)
  (cond
    ((= *local-client-id* (seat-get 0)) (+set-position (make-vec3 0.0 0.0 0.0) 0.0))
    ((= *local-client-id* (seat-get 1)) (+set-position (make-vec3 -0.5 0.0 0.0) 0.0))
    ((= *local-client-id* (seat-get 2)) (+set-position (make-vec3 0.5 0.0 0.0) 0.0))
    (#t (printf "Client-id ~a not in seat list\n" *local-client-id*))))
  
;----------------------
; execute-client-commands!
;----------------------
(define (set-scene! dest scene)
  (ss-pic-visible! "")
  (ss-destination! dest)
  (ss-scene! scene))

(define (execute-client-commands! cl)
  (define clstate (client-state cl))
  (define movenum (vector-ref clstate 0))
  (define cmd (vector-ref clstate 1))
  (define parm (vector-ref clstate 2))
  (when (> movenum (ss-cmd-num))
    (ss-cmd-num! movenum)
    (cond
      ((eq? cmd 'next)
       (printf "exec Swipe-forward\n")
       (when (not (= (ss-destination) dest-airport))
         (let ((num-scenes (length (list-ref *pic-list* (ss-destination)))))
           (if (= (ss-scene) (- num-scenes 1))
               (set-scene! dest-airport 0)
               (set-scene! (ss-destination) (+ 1 (ss-scene)))))))
       
      ((eq? cmd 'prev)
       (printf "exec Swipe-back\n")
       (when (not (= (ss-destination) dest-airport))
         (if (= (ss-scene) 0)
             (set-scene! dest-airport 0)
             (set-scene! (ss-destination) (- (ss-scene) 1)))))
      
      ((eq? cmd 'destination)
       (printf "exec destination\n")
       (set-scene! parm 0))
      
      ((eq? cmd 'pic)
       (printf "exec pic\n")
       (ss-pic-visible! parm))
      
      ))
  )

(define (match dest scene) (and (= (ss-destination) dest) (= (ss-scene) scene)))

;----------------------
; (time-sound wav volume yaw-position)
;
; Start a wav file a certain number of seconds after changing to this scene
; An optional volume can scale the sound.
; An optional yaw-degrees will spatialize the sound (can't be a stereo sample!).
;----------------------
(define (time-sound time wav . options)
  ; add this to the fetch-list for preloading on scene transition
  (when *fetch-gather*
    (set! *fetch-uri* (cons wav *fetch-uri*)))
        
  (define volume (if (> (length options) 0)
                     (list-ref options 0)
                     1.0))
  (when (crossed-seconds (+ time *transition-time*))
    (if (> (length options) 1)
        (+sound wav
                (opt-position (make-vec3
                               (- 0.0 (sin (degrees->radians (list-ref options 1))))
                               0.0
                               (- 0.0 (cos (degrees->radians (list-ref options 1))))))
                (opt-volume volume))        
        (+sound wav (opt-volume volume)))))

;----------------------
; background sound will be looped
;----------------------
(define (background-sound wav . options)
  ; add this to the fetch-list for preloading on scene transition
  (when *fetch-gather*
    (set! *fetch-uri* (cons wav *fetch-uri*)))
        
  (define volume (if (> (length options) 0)
                     (list-ref options 0)
                     1.0))
  (when (crossed-seconds (+ 0.01 *transition-time*))
    (+sound wav 'loop (opt-volume volume))))
  

; Return the yaw degrees of the current view.
; You can get a quick display for tuning parms with (+hud (floor (view-yaw)))
(define (view-yaw)
  (define fwd (mat4-forward *pose-inverse*))
  (radians->degrees (atan (- 0.0 (vec3-x fwd)) (- 0.0 (vec3-z fwd)))))

; Convenience function for 2D images - given the width and height of the
; original image in pixels, it will be centered and scaled to natural
; resolution, modified by the floating point s scale value.
(define (mat4-pic-scale w h s)
  (mat4-translate -0.5 -0.5 -0.5) 
  (mat4-scale/xyz (* 0.001 w) (* 0.001 h) 1))

; Given a 0.0 to 1.0 range, ramp the return value
; from 0.0 to 1.0 in the first 10%, then back down to 0.0
; in the last 10%.
(define (fade-in-out f)
  (cond
    ((< f 0.1) (* 10.0 f))
    ((> f 0.9) (* 10.0 (- 1.0 f)))
    (#t 1.0)))

;-----------------
; animate the swipe icon if the user has
; been sitting at the same scene for a long time.
;-----------------
(uri headset-uri  "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/2d/demo_headset.png")
(uri swipe-arrow-uri "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/2d/SwipeSuggestionArrowRight.png")

(define (swipe-anim first-time)
  (define repeat-time 10.0)
  (define anim-time 3.0)
    (when (and (> (- *script-seconds* *transition-time* ) first-time) (not (= (ss-destination) dest-airport)))
      (let* ( (cycle (/ (- *script-seconds* *transition-time* first-time) repeat-time))
              (prev-cycle (/ (- *prev-script-seconds* *transition-time* first-time) repeat-time))
              (cycle-frac (- cycle (floor cycle)))
              (anim-frac (* (/ repeat-time anim-time) cycle-frac )) )
      ; at the crossing time, get the current view direction
      (when (not (= (floor prev-cycle) (floor cycle)))
        (set! *swipe-anim-dir* (view-yaw))
        (printf "*swipe-anim-dir* ~a\n" *swipe-anim-dir*))
      (when (< 0.0 anim-frac 1.0)
        (let ((anim (* 0.1 (- *script-seconds* (floor *script-seconds*))))
              (fade (fade-in-out anim-frac)))
              (+quad headset-uri
                     (mat4-compose
                      (mat4-pic-scale 489 259 0.1)
                      (mat4-translate 0.0 -0.75 -1.8) 
                      (mat4-rotate-y (degrees->radians *swipe-anim-dir*)))
                     (opt-parm 1.0 1.0 1.0 fade))
              (+quad swipe-arrow-uri
                     (mat4-compose
                      (mat4-pic-scale 20 35 0.1)
                      (mat4-translate (+ 0.1 anim) -0.65 -1.79) 
                      (mat4-rotate-y (degrees->radians *swipe-anim-dir*)))
                     (opt-parm 1.0 1.0 1.0 fade))
              )))))

; Given two degree mesurements that might include
; negative or > 360.0 values, return the distance
; between them in the 0.0 to 180.0 range.
(define (delta-degrees x y)
  (define (fremainder v d)
    (- v (* d (floor (/ v d)))))
  (define z (fremainder (abs (- x y)) 360.0))
  (if (> z 180.0)
      (- 360.0 z)
      z))
  

;-----------------
; pic-button-yaw-pitch
;
; Place a single pic-button at the given yaw and pitch, then
; center the image if active.
;-----------------
(uri pic-button-uri "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/2d/K+ghost+view.png")
(uri pic-mini-button-uri "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/2d/point.png")

(define (pic-button-yaw-pitch yaw-degrees pitch-degrees pic-def)
  ; add this to the fetch-list for preloading on scene transition
  (when *fetch-gather*
    (set! *fetch-uri* (cons (list-ref pic-def 3) *fetch-uri*)))
        
  (define tip-text (list-ref pic-def 0))
  (define yaw (degrees->radians yaw-degrees))
  (define pitch (degrees->radians pitch-degrees))
  (define mat-yaw (mat4-rotate-y yaw))
  (define vec-right (mat4-transform-dir mat-yaw (make-vec3 1.0 0.0 0.0)))
  (define angles (mat4-compose
                  (mat4-rotate-y yaw)
                  (mat4-rotate-around vec-right pitch)))                  
  (define xform (mat4-compose 
                 (mat4-translate -0.5 -0.5 -0.5) 
                 (mat4-scale/xyz 0.2 0.2 0.01) 
                 (mat4-translate 0.0 0.0 -2.0)
                 angles))
  (define showing (string=? (ss-pic-visible) tip-text))

  (cond
    ; Draw the pic if active, using the underlay plane
    (showing   
     (let* (
            (w (list-ref pic-def 1))
            (h (list-ref pic-def 2))
            (scale (/ 2.0 (max w h)))
            (url (list-ref pic-def 3)))
       (+underlay url
              (mat4-compose  (mat4-translate -0.5 -0.5 0.0)
                             (mat4-scale/xyz (* scale w) (* scale h) 1.0)
                             (mat4-translate 0.0 0.0 -1.99)
                             angles))
       )
     ; if looking far degrees away, put the image away
     ; Don't do this in social, because other clients may
     ; be looking at it.
     (when (and (not social?) (> (delta-degrees (view-yaw) yaw-degrees) 80.0))
          (client-command! 'pic ""))

     )

    ; if any pic is up, hide all the other buttons
    ((> (string-length (ss-pic-visible)) 0) #f)

    ; Draw the icon and check for hit to bring up the pic
    (#t        
     (let ((gaze-now (gaze-on-bounds? bounds3-unit xform)))
           (+quad (if *gaze-close-this-test*
                      pic-button-uri
                      pic-mini-button-uri)
                  xform)
           
           ; tool tip if gazing on it
           (when gaze-now
             (+text tip-text
                    (mat4-compose 
                     (mat4-translate 0.0 0.15 -1.8)
                     angles))
             ; toggle active if press
             (when (pressed-action)
               (+sound WAV-ACTIVATE)
               (client-command! 'pic (if (string=? (ss-pic-visible) tip-text)
                                         ""
                                         tip-text))))))))
    

(define dest-airport        0)
(define dest-chichenitza    1)
(define dest-christredeemer 2)
(define dest-colosseum      3)
(define dest-giza           4)
(define dest-greatwall      5)
(define dest-machupicchu    6)
(define dest-petra          7)
(define dest-tajmahal       8)

; This list determines how many scenes are in each destination.  Swiping past the last
; scene returns to the airport.

; Most of these names could be generated by concatenating the scene number, but I assume
; we want the flexibility to swap things around without renaming files.
(define *pic-list*
  '(
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/AirportTerminal/terminal_2.jpg"
     )

    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_3.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_4.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_5.jpg"
     )
    
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2.jpg"
     )
    
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_3.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_4.jpg"
     )
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_3.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_4.jpg"
     )
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_2.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_3.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_4.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_5.jpg"
     )
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_3.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_4.jpg"
     )
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_3.jpg"
     )
    (
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_1.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2.jpg"
     "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_3.jpg"
     
     )))


;-----------------
; tic function
;-----------------
(define (tic)
  ; position social players
  (set-position)

  ; debug tool to see yaw angles for placement
  (when (held-left-trigger)
    (+hud (floor (view-yaw))))
  
  ; a swipe will signal the controlling client to change rooms
  (when (or (pressed-swipe-forward) (pressed-dpad-right))
    (printf "Swipe-forward\n")
    (client-command! 'next #f))
  (when (or (pressed-swipe-back) (pressed-dpad-left))
    (printf "Swipe-back\n")
    (client-command! 'prev #f))

  ; a tap not near any gaze button will clear pics
  (when (and (pressed-action)
             (not *gaze-on-last-frame*)
             (not (string=? "" (ss-pic-visible))))
    (client-command! 'pic ""))
  
  ; pressing the back button will go back to the airport, or exit the app
  (when (pressed-back)
    (back-handled)
    (if (match dest-airport 0)
        (+finish)
        (client-command! 'destination 0)))

  ; execute commands from each client
  (when (controlling-client?)
    (for-each execute-client-commands! *clients*))


  ; present the current room panorama
  (+pano (list-ref (list-ref *pic-list* (ss-destination))
                   (ss-scene)))

  ; reorient whenver we change scenes
  (if (not (and (= (ss-destination) *current-destination*)
                (= (ss-scene) *current-scene*)))
      (begin
        (+reorient)
        (set! *current-destination* (ss-destination))
        (set! *current-scene* (ss-scene))
        ; We want to fetch all the data needed for each scene before the first frame of the
        ; scene is drawn.  The background-sound, time-sound, and pic-button-yaw-pitch functions
        ; always add their URI to the *fetch-uri* list, even they aren't going to be drawn.
        (set! *fetch-gather* #t)
        (set! *fetch-uri* '()))
  
      ; We need to reset transition-time the frame after all the uri have been fetched, because
      ; an unknown amount of time is needed to ensure everything is cached.
      (when *fetch-gather*
        (set! *transition-time* *script-seconds*)
        (set! *fetch-gather* #f)))
      
  ; scene specific features
  (cond
    ;---------------
    ; airport
    ;---------------    
    ((match dest-airport 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_amb_airport_terminal_01.wav" 0.7)

     ; spatialized airplane sounds
     (time-sound 2.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/t_evt_plane_takeoff_01.wav" 1.0 90.0)
     (time-sound 7.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/t_evt_plane_takeoff_02.wav" 1.0 -90.0)
     (time-sound 12.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/t_evt_plane_takeoff_03.wav" 1.0 180.0)
     (time-sound 15.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/t_evt_plane_takeoff_04.wav" 1.0 -33.0)
     (time-sound 22.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/t_evt_plane_takeoff_05.wav" 1.0 45.0)
     (time-sound 28.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/t_evt_plane_takeoff_06.wav" 1.0 180.0)

     ; play the "welcome" clip only on initial launch, not every time you return to the airport
     (when (crossed-seconds 3.0)
       (+sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_welcome_01.wav"))

     ; draw the departure board model
     (+model "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/Models/flightboard_main.ovrscene"
             (mat4-translate 0.0 0.5 -2.1))

     ; Draw the share button on the board
     (share-button)
          
     ; add all the buttons
     (departure-button-list
      '(
        ( "Chichen Itza"            chichenitza-pics )
        ( "Christ the Redeemer"     christredeemer-pics )
        ( "Colosseum"               colosseum-pics )
        ( "Great Pyramid of Giza"   giza-pics )
        ( "Great Wall of China"     greatwall-pics )
        ( "Machu Picchu"            machupicu-pics )
        ( "Petra"                   petra-pics )
        ( "Taj Mahal"               tajmahal-pics )
        )))

    ;---------------
    ; Christ Redeemer
    ;---------------    
    ((match dest-christredeemer 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 2.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_rio_01.wav")
     (time-sound 7.3 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_rio_02.wav")
     (swipe-anim 17.0)
     (pic-button-yaw-pitch 5.0 -10.0 '( "Construction 1926-30"     436  700 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_construction1926-30.jpg" ))
     (pic-button-yaw-pitch 0.0 35.0 '( "On Face closeup"     1920 1440 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_face.jpg" ))
     (pic-button-yaw-pitch 100.0 20.0 '( "Redeemer from afar"     2560 1711 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_afar.jpg" ))
     (pic-button-yaw-pitch -22.0 -10.0 '( "NOTE - Construction"     768  1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_text_cost.jpg" ))
     (pic-button-yaw-pitch 40.0 -10.0 '( "NOTE - Creator"     768  1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_text_creator.jpg" ))
     )
    ((match dest-christredeemer 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/bg_1.wav")
     (time-sound 0.7 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_rio_04.wav")
     (time-sound 13.5 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_rio_03.wav")
     (swipe-anim 25.0)
     (pic-button-yaw-pitch -172.0 20.0 '( "Lightning Damage Jan 2014"     2560 1711 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_lightning.jpg" ))
     (pic-button-yaw-pitch 168.0 10.0 '( "NOTE - Dimensions"     768  1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_text_dimensions.jpg" ))
     (pic-button-yaw-pitch 180.0 -10.0 '( "NOTE - Levitation"     768  1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/christredeemer_2d_text_levitate.jpg" ))
     )

    ;---------------
    ; Colosseum
    ;---------------    
    ((match dest-colosseum 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 2.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_colosseum_01.wav")
     (time-sound 7.3 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_colosseum_02.wav")
     (swipe-anim 18.0)
     (pic-button-yaw-pitch 45.0 20.0 '( "Colosseum from afar"     770 430 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_afar.jpg" ))
     (pic-button-yaw-pitch -35.0 10.0 '( "NOTE - Visitors"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_text_tourists.jpg" ))
     )
    ((match dest-colosseum 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.5 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_colosseum_03.wav")
     (swipe-anim 12.0)
     (pic-button-yaw-pitch 60.0 10.0 '( "Relief - Gladiator vs Beast"     500 413 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_reliefgladvsbeast.jpg" ))
     (pic-button-yaw-pitch -15.0 10.0 '( "NOTE - Entrances"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_text_entrances.jpg" ))
     )
    ((match dest-colosseum 2)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 0.1 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_walla_crowd_stadium.wav" 0.7)
     (time-sound 3.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_colosseum_04.wav")
     (time-sound 11.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_colosseum_05.wav")
     (time-sound 13.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_walla_crowd_stadium_gladiator_battle.wav" 0.7)
     (swipe-anim 20.0)
     (pic-button-yaw-pitch -75.0 10.0 '( "Detail Shot"    1024 683 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_detail.jpg" ))
     (pic-button-yaw-pitch 0.0 -15.0 '( "NOTE - Ship Battles"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_text_shipbattles.jpg" ))

     )
    ((match dest-colosseum 3)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_colosseum_06.wav")
     (swipe-anim 18.0)
     (pic-button-yaw-pitch -15.0 10.0 '( "Mosaic - Spectacles"     919 1321 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_mosaic_gladiator2.jpg" ))
     (pic-button-yaw-pitch -40.0 10.0 '( "NOTE - Animal Wipeout"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/colosseum_2d_text_animalwipeout.jpg" ))
     )

    ;---------------
    ; Taj Mahal
    ;---------------    
    ((match dest-tajmahal 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/bg_1.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_taj_01.wav")
     (swipe-anim 11.0)
     (pic-button-yaw-pitch -65.0 25.0 '( "Taj Mahal from Afar"     700 464 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_afar.jpg" ))
     (pic-button-yaw-pitch 5.0 14.0 '( "Building Facade Detail"     800 521 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_detail2.jpg" ))
     (pic-button-yaw-pitch -6.0 14.0 '( "Archway Detail"     1023 685 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_detail5.jpg" ))
     (pic-button-yaw-pitch 175.0 10.0 '( "Mosque Interior Archways"     1280 960 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_mosquearchways.jpg" ))
     (pic-button-yaw-pitch 0.0 -6.0 '( "The Tombs"     798 307 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_tomb.jpg" ))
     )
    ((match dest-tajmahal 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/bg_2.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_taj_02.wav")
     (swipe-anim 15.0)
     
     (pic-button-yaw-pitch 25.0 10.0 '( "NOTE - Plans for Dark Copy"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_text_blackcopy.jpg" ))
     (pic-button-yaw-pitch -23.0 10.0 '( "NOTE - Dimensions"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_text_height.jpg" ))
     (pic-button-yaw-pitch -13.0 10.0 '( "NOTE - Costs"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_text_cost.jpg" ))
     (pic-button-yaw-pitch 15.0 10.0 '( "NOTE - Construction"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_text_construction.jpg" ))
     )   
    ((match dest-tajmahal 2)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 0.5 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_taj_03.wav")
     (swipe-anim 10.0)
     (pic-button-yaw-pitch 0.0 10.0 '( "Jeweled Inlay"     1600 1200 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_detail.jpg" ))
     (pic-button-yaw-pitch -45.0 50.0 '( "Minaret Detail"     1200 1600 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_detail4.jpg" ))
     (pic-button-yaw-pitch -60.0 10.0 '( "Building Corner Detail"     550 413 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_detail3.jpg" ))
     (pic-button-yaw-pitch -25.0 10.0 '( "NOTE - Jewels Stolen & Restoration"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/tajmahal_2d_text_stolen.jpg" ))
     )

    ;---------------
    ; Chichen Itza 
    ;---------------    
    ((match dest-chichenitza 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_ci_01.wav")
     (time-sound 7.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_ci_02.wav")
     (swipe-anim 18.0)
     (pic-button-yaw-pitch -30.0 20.0 '( "Chichen Itza from afar"     1500 1000 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_afar.jpg" ))
     (pic-button-yaw-pitch 141.0 15.0 '( "Carving Detail"     550 368 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_detailskulls.jpg" ))
     (pic-button-yaw-pitch 22.0 10.0 '( "NOTE - Name Origins"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_text_name.jpg" ))
     )
    ((match dest-chichenitza 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/bg_1.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_ci_03.wav")
     (swipe-anim 9.0)
     (pic-button-yaw-pitch 175.0 10.0 '( "Detailed Carving"     550 229 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_carving.jpg" ))
     (pic-button-yaw-pitch -10.0 10.0 '( "Steps Detail"     1600 923 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_detailsteps.jpg" ))
     (pic-button-yaw-pitch 4.0 17.0 '( "Building Detail"     3216 2136 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_detail.jpg" ))
     (pic-button-yaw-pitch 115.0 10.0 '( "NOTE - Paved Roads"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_text_pavedroads.jpg" ))
     )   
    ((match dest-chichenitza 2)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 0.1 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_ci_06.wav")
     (swipe-anim 9.0)
     (pic-button-yaw-pitch -10.0 10.0 '( "Mesoamerican Ballgame Modern Painting"     350 267 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_mesoamericanballgameartist.jpg" ))
     (pic-button-yaw-pitch 10.0 10.0 '( "Mesoamerican Ballgame Ancient Painting"     1461 1211 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_ballgamepainting.jpg" ))
     )
    ((match dest-chichenitza 3)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 0.5 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_ci_04.wav")
     (swipe-anim 10.0)
     (pic-button-yaw-pitch -10.0 10.0 '( "Statue of god Chac Mool"     500 375 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_statuegodchac-mool.jpg" ))
     )
    ((match dest-chichenitza 4)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/bg_2.wav")
     (time-sound 0.5 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_ci_05.wav")
     (swipe-anim 9.0)
     (pic-button-yaw-pitch 80.0 10.0 '( "NOTE - Civil War"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/chichenitza_2d_text_civilwar.jpg" ))
     )
     
    ;---------------
    ; Great Wall
    ;---------------    
    ((match dest-greatwall 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/bg_2.wav")
     (time-sound 1.5 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_gwc_01.wav")
     (time-sound 12.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_gwc_03.wav")
     (swipe-anim 18.0)
     (pic-button-yaw-pitch -30.0 30.0 '( "The Great Wall from the Sky"     800 577 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_2d_afar.jpg" ))
     (pic-button-yaw-pitch 35.0 10.0 '( "A Side View"     2582 619 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_2d_sideview.jpg" ))
     )
    ((match dest-greatwall 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_gwc_02.wav")
     (swipe-anim 12.0)
     (pic-button-yaw-pitch 0.0 -10.0 '( "One of the many gates"     2208 1244 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_2d_gateJiquan.jpg" ))
     )   
    ((match dest-greatwall 2)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/bg_1.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_gwc_04.wav")
     (swipe-anim 20.0)
     )
    ((match dest-greatwall 3)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_gwc_05.wav")
     (swipe-anim 8.0)
     (pic-button-yaw-pitch -5.0 -10.0 '( "NOTE - Bricks"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/greatwall_2d_text_bricks.jpg" ))
     )
    ((match dest-greatwall 4)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_gwc_06.wav")
     (swipe-anim 14.0)
     )

    ;---------------
    ; Giza
    ;---------------    
    ((match dest-giza 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_01.wav")
     (time-sound 12.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_04.wav")
     (swipe-anim 27.0)
     (pic-button-yaw-pitch 0.0 30.0 '( "View from Space"     858 536 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_fromspace.jpg" ))
     (pic-button-yaw-pitch 20.0 10.0 '( "NOTE - Cornerstone Construction"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_text_cornerstone.jpg" ))
     )
    ((match dest-giza 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_03.wav")
     (time-sound 17.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_02.wav")
     (swipe-anim 28.0)
     (pic-button-yaw-pitch -60.0 10.0 '( "View from the City"     1200 740 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_frommoderncity.jpg" ))
     (pic-button-yaw-pitch -5.0 10.0 '( "NOTE - Concave Sides"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_text_concave.jpg" ))
     (pic-button-yaw-pitch -5.0 25.0 '( "NOTE - Orion's Belt"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_text_orion.jpg" ))
     )
    ((match dest-giza 2)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_05.wav")
     (time-sound 18.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_06.wav")
     (swipe-anim 28.0)
     (pic-button-yaw-pitch 52.0 10.0 '( "Entrance"     1024 812 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_entrance.jpg" ))
     (pic-button-yaw-pitch 42.0 10.0 '( "Grand Gallery"     605 412 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_grandgallery.jpg" ))
     (pic-button-yaw-pitch 32.0 10.0 '( "Interior Details"     640 427 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_detail.jpg" ))
     (pic-button-yaw-pitch 22.0 10.0 '( "Kings Chamber"     640 408 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_kingschamber.jpg" ))
     )
    ((match dest-giza 3)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_07.wav")
     (time-sound 12.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_08.wav")
     (time-sound 29.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_giza_09.wav")
     (swipe-anim 34.0)
     (pic-button-yaw-pitch -40.0 10.0 '( "Sphinx"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_text_sphinx.jpg" ))
     (pic-button-yaw-pitch 18.0 10.0 '( "NOTE - Weight and Cost"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/giza_2d_text_weightcost.jpg" ))
     )

    ;---------------
    ; Machu Picchu
    ;---------------    
    ((match dest-machupicchu 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 2.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_mp_01.wav")
     (time-sound 7.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_mp_02.wav")
     (swipe-anim 18.5)
     (pic-button-yaw-pitch -60.0 20.0 '( "Machu Picchu from Afar"     800 577 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_afar.jpg" ))
     (pic-button-yaw-pitch -10.0 -10.0 '( "Carved Steps"     683 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_carvedsteps.jpg" ))
     (pic-button-yaw-pitch 0.0 -10.0 '( "Unique Flying Steps"     683 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_flyingsteps.jpg" ))
     (pic-button-yaw-pitch 10.0 -10.0 '( "Plumbing"     300 200 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_plumbing.jpg" ))
     )
    ((match dest-machupicchu 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 2.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_mp_03.wav")
     (time-sound 10.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_mp_04.wav")
     (swipe-anim 18.5)
     (pic-button-yaw-pitch -51.0 30.0 '( "Top View of Architecture"     800 600 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_architecturefromtop.jpg" ))
     (pic-button-yaw-pitch -40.0 10.0 '( "NOTE - Ashlar Construction"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_text_ashlar.jpg" ))
     )
    ((match dest-machupicchu 2)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 2.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_mp_05.wav")
     (time-sound 11.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_mp_06.wav")
     (swipe-anim 17.0)
     (pic-button-yaw-pitch 85.0 -30.0 '( "The Steps Up"     350 525 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_stepsup.jpg" ))
     )
    ((match dest-machupicchu 3)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 2.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_mp_07.wav")
     (swipe-anim 18.0)
     (pic-button-yaw-pitch 10.0 10.0 '( "Torreon"     3888 2424 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_torreon.jpg" ))
     (pic-button-yaw-pitch -10.0 10.0 '( "NOTE - Astronomy"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/machupicchu_2d_text_astronomy.jpg" ))
     )

    ;---------------
    ; Petra
    ;---------------    
    ((match dest-petra 0)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 0.5 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_petra_01.wav")
     (time-sound 12.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_petra_02.wav")
     (swipe-anim 22.0)
     (pic-button-yaw-pitch 7.0 50.0 '( "Detail Work Treasury"     533 800 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_detail1.jpg" ))
     (pic-button-yaw-pitch -24.0 28.0 '( "Detail Work Column"     525 350 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_detail2.jpg" ))
     (pic-button-yaw-pitch 0.0 10.0 '( "Inside View"     1024 344 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_inside.jpg" ))
     (pic-button-yaw-pitch -80.0 10.0 '( "NOTE - Another Name"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_text_rosecity.jpg" ))
     )
    ((match dest-petra 1)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_petra_03.wav")
     (time-sound 11.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_petra_04.wav")
     (swipe-anim 22.0)
     (pic-button-yaw-pitch 0.0 35.0 '( "Theater from above"     432 288 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_theater.jpg" ))
     (pic-button-yaw-pitch -85.0 10.0 '( "NOTE - Weathering"     768 1024 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_text_wethering.jpg" ))
     )
    ((match dest-petra 2)
     (background-sound "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/lp_background_generic_01.wav")
     (time-sound 1.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_petra_05.wav")
     (time-sound 7.0 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/audio/vo_exp_petra_06.wav")
     (swipe-anim 16.0)
     (pic-button-yaw-pitch -35.0 -7.0 '( "Map"     900 600 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_map.jpg" ))
     (pic-button-yaw-pitch 5.0 10.0 '( "Monastary"     990 1487 "https://s3.amazonaws.com/o.oculuscdn.com/netasset/matt_test/New7Wonders/petra_2d_monastary.jpg" ))
     )
    
    )
  
  ; If we just transitioned to this scene, force everything that will be used to
  ; load before rendering it.
  (when *fetch-gather*
    (apply +fetch *fetch-uri*))
  
  )


(remote "172.22.52.182" #f tic)
