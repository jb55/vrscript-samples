#lang racket/base
(require "remote.rkt")
; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

(uri WAV-LULLABY  "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/mafia_lullaby.wav")

;-----------------
; These procedures are different in racket/base and in chibi's r7rs, so re-defining them here.
;-----------------
(define (remov3 obj lst)
  (filter (lambda (item) (not (equal? obj item))) lst))

(define (remov3* lst1 lst2)
  (filter (lambda (obj) (not (member obj lst1))) lst2))

(define (empty? lst) (= 0 (length lst)))

(define (andm4p proc lst)
  (empty? (filter (lambda (obj) (not (proc obj))) lst)))

;-----------------
; Server State
; (game-mode player-state-1 player-state-2 ...)
; game-mode: symbol, can be one of WAITING, PLAYING, WIN-KILLERS or WIN-VILLAGERS
; player-state-i: state of a player, see below
;
; Player State
; (id role is-dead is-exiled is-awake)
; id: integer, player id
; role: symbol, role assigned to this player (god, killer, angel, detective or villager)
; is-dead: boolean, means player can't talk but can see everything (doesn't sleep)
; is-exiled: boolean, means player can't talk but still sleeps/wakes-up
; is-awake: boolean, means player can see what's happening (as opposed to a black screen when sleeping)
;-----------------

(define server-parms '(8 #t "Mafia" "https://s3.amazonaws.com/o.oculuscdn.com/v/test/social/reversi.jpg"))

; Local server state for testing
;(define *local-player-id* 1)
;(define *local-server-state* (list "WAITING"
;                                   '(1 "GOD" #f #f #t)
;                                   '(2 "ANGEL" #f #f #t)
;                                   '(3 "KILLER" #f #f #t)
;                                   '(4 "VILLAGER" #f #f #t)
;                                   '(5 "DETECTIVE" #f #f #t)))
;(define *local-client-positions* (list (list 1 (make-vec3 0.0 0.0 0.0))
;                                       (list 2 (make-vec3 2.0 0.0 -1.5))
;                                       (list 3 (make-vec3 2.0 0.0 -3.0))
;                                       (list 4 (make-vec3 -2.0 0.0 -3.0))
;                                       (list 5 (make-vec3 -2.0 0.0 -1.5))))

(set-server-state! (list 'WAITING (list *local-client-id* 'GOD #f #f #t)))

(define (get-game-mode) (car *server-state*))
(define (get-player-states) (cdr *server-state*))
(define (get-player-ids) (map car (get-player-states)))

(define (get-player-state player-id) (assoc player-id (get-player-states)))
(define (get-player-role player-id) (cadr (get-player-state player-id)))

(define (is-god? player-id) (eq? 'GOD (get-player-role player-id)))
(define (is-angel? player-id) (eq? 'ANGEL (get-player-role player-id)))
(define (is-killer? player-id) (eq? 'KILLER (get-player-role player-id)))
(define (is-villager? player-id) (eq? 'VILLAGER (get-player-role player-id)))
(define (is-detective? player-id) (eq? 'DETECTIVE (get-player-role player-id)))

(define (is-dead? player-id) (list-ref (get-player-state player-id) 2))
(define (is-exiled? player-id) (list-ref (get-player-state player-id) 3))
(define (is-awake? player-id) (list-ref (get-player-state player-id) 4))

(define (set-game-mode! new-mode)
  (set-server-state! (cons new-mode (cdr *server-state*))))
(define (set-player-states! new-states)
  (set-server-state! (cons (car *server-state*) new-states)))
(define (set-player-state! new-state)
  (define player-id (car new-state))
  (set-player-states! (cons new-state (remov3 (get-player-state player-id) (get-player-states)))))

(define (set-player-state-value! player-id value-pos new-value)
  (set-player-state! (list player-id
                           (if (= value-pos 1) new-value (get-player-role player-id))
                           (if (= value-pos 2) new-value (is-dead? player-id))
                           (if (= value-pos 3) new-value (is-exiled? player-id))
                           (if (= value-pos 4) new-value (is-awake? player-id)))))

(define (set-player-role! player-id new-role) (set-player-state-value! player-id 1 new-role))

(define (kill-player! player-id) (set-player-state-value! player-id 2 #t))
(define (awake-player! player-id) (set-player-state-value! player-id 4 #t))
(define (set-player-dead! player-id v) (set-player-state-value! player-id 2 v))
(define (set-player-exiled! player-id v) (set-player-state-value! player-id 3 v))
(define (set-player-awake! player-id v) (set-player-state-value! player-id 4 v))

(define (set-everyone-awake!)
  (for-each (lambda (player-state) (set-player-awake! (car player-state) #t)) (get-player-states)))
(define (set-everyone-asleep!)
  (for-each (lambda (player-state) (set-player-awake! (car player-state) #f)) (get-player-states)))

(define (reset-player-state! player-id)
  (set-player-dead! player-id #f)
  (set-player-exiled! player-id #f)
  (set-player-awake! player-id #t))
(define (reset-game!)
  (set-game-mode! 'WAITING)
  (for-each reset-player-state! (get-player-ids)))

(define (add-player! player-id)
  (set-player-state! (list player-id 'VILLAGER #f #f #t)))
(define (remove-player! player-id)
  (set-player-states! (remov3 (get-player-state player-id) (get-player-states))))

; TODO: try to get rid of this
(define *has-started* #f)
(define (start-game!)
  (set-game-mode! 'WAITING)
  (set-player-states! (list (list *local-client-id* 'GOD #f #f #t))))

;-----------------
; Util UI components.
;-----------------
(define (get-rot-y position)
  (define player-position (mat4-origin (list->mat4 (client-pose (client-by-id *local-client-id*)))))
  (define dx (- (vec3-x position) (vec3-x player-position)))
  (define dz (- (vec3-z position) (vec3-z player-position)))
  (define rad (atan (/ (abs dz) (abs dx))))
  (define op1 (if (< dx 0) + -))
  (define op2 (if (< dz 0) - +))
  (if (< (abs dx) 0.0001) 0.0 (op1 0.0 (op2 pi/2 rad))))

(define (draw-text! text position scale color)
  (define rot-y (get-rot-y position))
  (+text text
         (mat4-compose (mat4-scale scale) (mat4-rotate-y rot-y) (mat4-translatev position))
         (opt-parmv color)))

(define (draw-instructions! text)
  (draw-text! text (make-vec3 0.0 -0.2 0.0) 2.0 (make-vec4 1.0 1.0 0.5 1.0)))

; Draw text on top of player's head
(define (draw-player-text! player-id text height scale color)
  ;(define player-position (cadr (assoc player-id *local-client-positions*)))
  ; TODO: maybe use seat position instead, so text doesn't move as users look around
  (define player-position (mat4-origin (list->mat4 (client-pose (client-by-id player-id)))))
  (define text-position (make-vec3 (vec3-x player-position) height (vec3-z player-position)))
  (draw-text! text text-position scale color))

(define (draw-button! text position is-selected on-click)
  (define bounds-trans (mat4-compose (mat4-translate -0.5 -0.3 -0.5)
                                     (mat4-scale/xyz 1.0 0.15 0.15)
                                     (mat4-rotate-y (get-rot-y position))
                                     (mat4-translatev position)))

  (define gaze-now (gaze-on-bounds? bounds3-unit bounds-trans))

  ; Highlightable text
  (draw-text! text position 2.0 (cond (gaze-now (make-vec4 1.0 1.0 0.5 1.0))
                                      (is-selected (make-vec4 0.5 1.0 0.5 1.0))
                                      (else (make-vec4 0.5 0.5 1.0 1.0))))

  ; Run on-click procedure if button is pressed
  (when (and (pressed-action) gaze-now) (on-click)))

; Draw button on top of player's head
(define (draw-player-button! player-id text height is-selected on-click)
  ;(define player-position (cadr (assoc player-id *local-client-positions*)))
  (define player-position (mat4-origin (list->mat4 (client-pose (client-by-id player-id)))))
  (define button-position (make-vec3 (vec3-x player-position) height (vec3-z player-position)))
  (draw-button! text button-position is-selected on-click))

;-----------------
; God - Lobby
; God will need to assign all required roles and start the game.
;-----------------
(define (draw-role-button! player-id role height)
  (draw-player-button! player-id (symbol->string role) height
                       (eq? role (get-player-role player-id))
                       (lambda () (set-player-role! player-id role))))

(define (draw-role-buttons! player-id)
  (for-each
   (lambda (role height) (draw-role-button! player-id role height))
   '(VILLAGER DETECTIVE KILLER ANGEL) '(0.25 0.45 0.65 0.85)))

(define (draw-start-button!)
  (draw-button! "Start Game" (make-vec3 0 -0.5 0) #f (lambda () (set-game-mode! 'PLAYING))))

(define (tic-god-waiting)
  ; Reset game on first load
  (unless *has-started* (start-game!) (set! *has-started* #t))

  ; Explanation text
  (draw-instructions! "You're God!\nSelect players' roles before starting the game.")

  ; Add newcomers to server state
  (for-each add-player! (remov3* (get-player-ids) (map client-id *clients*)))

  ; Buttons of role selection on top of players' heads
  (for-each
   (lambda (player-id) (draw-role-buttons! player-id))
   (remov3 *local-client-id* (get-player-ids)))

  ; Start button. Must have at least one killer to start the game.
  ; TODO: actually need to have at least 2 other villagers otherwise killer wins.
  (when (not (empty? (filter is-killer? (get-player-ids)))) (draw-start-button!)))

;-----------------
; God - Playing - Day
; When the day starts, god may announce any deaths that happened during the night (and will need to
; click the appropriate button to kill that player). At the end of the day, god may also exile a
; villager (by clicking on the correct button above that player) if the city voted for it.
;-----------------
(define (draw-kill-button! player-id)
  (draw-player-button! player-id "Kill" 0.70 #f (lambda () (kill-player! player-id))))

(define (draw-exile-button! player-id)
  (draw-player-button! player-id "Exile" 0.50 #f (lambda () (set-player-exiled! player-id #t))))

(define (draw-bring-back-button! player-id)
  (define (on-click) (set-player-exiled! player-id #f))
  (draw-player-button! player-id "Bring Back" 0.50 #f on-click))

; Villagers win when all killers are dead or exiled
(define (have-villagers-won?)
  (andm4p (lambda (player-id) (or (is-dead? player-id) (is-exiled? player-id)))
          (filter is-killer? (get-player-ids))))

; Killers win when the number of non-killers is less or equal the number killers
(define (have-killers-won?)
  (define active-villagers
    (filter (lambda (p) (not (or (is-god? p) (is-dead? p) (is-exiled? p)))) (get-player-ids)))
  (<= (length active-villagers) (* 2 (length (filter is-killer? active-villagers)))))

(define (tic-god-day)
  (define villagers (remov3 *local-client-id* (get-player-ids)))
  (define alive-villagers (filter (lambda (player-id) (not (is-dead? player-id))) villagers))
  (define exiled-villagers (filter is-exiled? alive-villagers))
  (define non-exiled-villagers (remov3* exiled-villagers alive-villagers))

  ; Draw Kill button above all alive villagers
  (for-each draw-kill-button! alive-villagers)

  ; Draw "Exile" button above all alive non-exiled villagers
  (for-each draw-exile-button! non-exiled-villagers)

  ; Draw "Bring Back" above the exiled ones
  (for-each draw-bring-back-button! exiled-villagers)

  ; Check end of the game
  (cond ((have-killers-won?) (set-game-mode! 'WIN-KILLERS))
        ((have-villagers-won?) (set-game-mode! 'WIN-VILLAGERS)))

  ; Put everyone to sleep when button clicked
  (draw-button! "Start Night" (make-vec3 0.0 -0.5 0.0) #f set-everyone-asleep!))

;-----------------
; God - Playing - Night
; During the night, god may awake players with special roles to gather their actions.
;-----------------
(define (draw-awake-button! text height player-ids)
  (draw-button! text (make-vec3 0.0 height 0.0) #f (lambda () (for-each awake-player! player-ids))))

; Receives a list of buttons' data containing the button's text and list of players in that role.
; Recursively draws the buttons, from bottom to top, adjusting the height accordingly.
(define (draw-awake-buttons-rec! buttons-data base-height)
  (when (not (empty? buttons-data))
    (let* ((current (car buttons-data)) (current-has-ids (not (empty? (cadr current)))))
      (when current-has-ids (draw-awake-button! (car current) base-height (cadr current)))
      (draw-awake-buttons-rec! (cdr buttons-data) (+ base-height (if current-has-ids 0.2 0.0))))))

(define (draw-awake-buttons! actionable-players)
  (define buttons-data (list (list "Awake Detectives" (filter is-detective? actionable-players))
                             (list "Awake Angels" (filter is-angel? actionable-players))
                             (list "Awake Killers" (filter is-killer? actionable-players))))
  (draw-awake-buttons-rec! buttons-data -0.3))

(define (draw-stare! player-id)
  (define player-pose (list->mat4 (client-pose (client-by-id player-id))))
  (define origin (mat4-origin player-pose))
  (define forward (mat4-forward player-pose))
  (define x0 (vec3-x origin))
  (define z0 (vec3-z origin))
  (define x1 (+ x0 (vec3-x forward)))
  (define z1 (+ z0 (vec3-z forward)))
  (define a (+ (* (- x1 x0) (- x1 x0)) (* (- z1 z0) (- z1 z0))))
  (define b (* 2 (+ (* x0 (- x1 x0)) (* z0 (- z1 z0)))))
  (define c (- (+ (* x0 x0) (* z0 z0)) 4))
  (define delta (- (* b b) (* 4 (* a c))))
  (define t (/ (+ (- b) (sqrt delta)) (* 2 a)))
  (define xf (+ x0 (* t (vec3-x forward))))
  (define zf (+ z0 (* t (vec3-z forward))))
  (define position (make-vec3 xf -0.7 zf))
  (define bounds-trans (mat4-compose (mat4-scale/xyz 0.5 0.9 0.5)
                                     (mat4-translatev position)))
  (+model "_bounds" bounds-trans (opt-parm 1.0 0.0 0.0 1.0)))

(define (tic-god-night)
  ; Players who can awake: not a villager, not dead and not exiled
  (define actionable-players
    (filter (lambda (p) (not (or (is-dead? p) (is-exiled? p) (is-villager? p))))
            (remov3 *local-client-id* (get-player-ids))))

  ; If there are awaken players, only show "Put to Sleep" button.
  ; Otherwise show buttons to awake specific roles.
  (if (> (length (filter is-awake? actionable-players)) 0)
      (begin
        (for-each draw-stare! (filter is-awake? actionable-players))
        (draw-button! "Put to Sleep" (make-vec3 0.0 -0.5 0.0) #f set-everyone-asleep!))
      (begin
        (draw-awake-buttons! actionable-players)
        (draw-button! "Start Day" (make-vec3 0.0 -0.5 0.0) #f set-everyone-awake!))))

;-----------------
; God - Playing
; God can see players' roles and act on players' states depending on the time of the day.
;-----------------
(define (draw-player-role! player-id)
  (draw-player-text! player-id (get-player-role player-id) 0.35 1.5 (make-vec4 1. 1. 1. 1.)))

(define (tic-god-playing)
  ; Draw players' roles
  (for-each draw-player-role! (remov3 *local-client-id* (get-player-ids)))

  ; Check whether it's day (everyone awake) or night
  (if (andm4p is-awake? (get-player-ids)) (tic-god-day) (tic-god-night)))

;-----------------
; Humans - Lobby
; Not really much to do while waiting for other players or waiting for God to assign roles.
;-----------------
(define (tic-human-waiting) (draw-instructions! "Waiting for other players.."))

;-----------------
; Humans - Playing
; Dead players can't talk, but can see everything.
; Exiled players can't talk, but sleep as usual.
; While asleep, players can't see.
;-----------------
; TODO: add some stars, use +geometry to make it a globe
(+shader "nightsky"
"
uniform highp mat4 Mvpm;
attribute highp vec4 Position;
void main()
{
  gl_Position = Mvpm * Position;
}
"
"
void main()
{
  gl_FragColor = vec4(vec3(0.0), 1.0);
}
")

(define (draw-sleep-box-wall! sx sy rx ry tx ty tz)
  ; TODO: Grrrr
  (define seat-num (- (length *client-seats*) (length (member *local-client-id* *client-seats*))))
  (define angle (+ (if (odd? seat-num) pi 0)
                   (* (quotient seat-num 2) (/ pi*2 (length *client-seats*)))))
  (+quad "_background" (mat4-compose (mat4-translate -0.5 -0.5 0.0)
                                     (mat4-scale/xyz sx sy 0)
                                     (mat4-rotate-x rx)
                                     (mat4-rotate-y ry)
                                     (mat4-translate tx ty tz)
                                     (mat4-rotate-y angle))
         (opt-shader "nightsky")))

(define (draw-sleep-box!)
  (draw-sleep-box-wall! 6.0 2.0 0.0 (* -10 pi/180) 0.0 0.0 0.8) ; Right
  (draw-sleep-box-wall! 6.0 2.0 0.0 (* 10 pi/180) 0.0 0.0 -0.8) ; Left
  (draw-sleep-box-wall! 6.0 2.0 pi/2 0.0 0.0 0.5 0.0)           ; Up
  (draw-sleep-box-wall! 6.0 2.0 pi/2 0.0 0.0 -0.5 0.0)          ; Down
  (draw-sleep-box-wall! 2.0 1.0 0.0 pi/2 0.5 0.0 0.0)           ; Front
  (draw-sleep-box-wall! 1.0 1.0 0.0 pi/2 -2.4 0.0 0.0))         ; Back

(define (tic-human-playing)
  (unless *has-started* (set! *has-started* #t))

  ; Show player's role
  (draw-instructions!
   (format "\n\nYou are a: ~a" (symbol->string (get-player-role *local-client-id*))))

  ; Player states
  (if (is-dead? *local-client-id*)
      ; Dead player
      (draw-instructions! "You're dead and can't talk!")

      ; Alive
      (begin

        ; Exiled player can't talk. TODO: mute player
        (when (is-exiled? *local-client-id*) (draw-instructions! "You're exiled and can't talk!"))

        ; Sleeping player can't see
        (when (not (is-awake? *local-client-id*))
              (draw-sleep-box!)
              (draw-instructions! "\nYou're sleeping!")))))

;-----------------
;
;-----------------
(define (tic-god)
  ; Remove from the server state the players who have quit
  (for-each
   (lambda (player-id) (when (not (client-by-id player-id)) (remove-player! player-id)))
   (get-player-ids))

  (cond ((eq? 'WAITING (get-game-mode)) (tic-god-waiting))
        ((eq? 'PLAYING (get-game-mode)) (tic-god-playing))
        (else (draw-button! "Restart Game" (make-vec3 0 -0.5 0) #f reset-game!))))

(define (tic-human)
  (if (eq? 'WAITING (get-game-mode)) (tic-human-waiting) (tic-human-playing)))

;-----------------
; tic
;
;-----------------
(define (draw-player-status! player-id)
  (define status (cond ((is-god? player-id) "(God)")
                       ((is-dead? player-id) "(Dead)")
                       ((is-exiled? player-id) "(Exiled)")
                       (else #f)))
  (when status (draw-player-text! player-id status 0.25 1.0 (make-vec4 1.0 1.0 1.0 1.0))))

(define (draw-share-server-button!)
  (define (on-click) (apply +share-server server-parms))
  (draw-button! "Share Server" (make-vec3 0.0 -0.25 -2.0) #f on-click))

; Position players in a circle. Seat #1 is opposite to seat #0, #3 is opposite to #2 and so on.
(define (set-player-position!)
  (define seat-num (- (length *client-seats*) (length (member *local-client-id* *client-seats*))))
  (define angle (+ (if (odd? seat-num) pi 0)
                   (* (quotient seat-num 2) (/ pi*2 (length *client-seats*)))))
  (+set-position (make-vec3 (- (* 2 (cos angle))) 0.0 (* 2 (sin angle))) (- angle pi/2)))

(define (tic-connected)
  ; Print end of the game
  (cond ((eq? 'WIN-KILLERS (get-game-mode)) (draw-instructions! "\nKillers won!"))
        ((eq? 'WIN-VILLAGERS (get-game-mode)) (draw-instructions! "\nVillagers won!")))

  ; Position player in a circle
  (set-player-position!)

  ; Player-specific logic
  (if (controlling-client?) (tic-god) (tic-human))

  ; Draw players' status (exiled/dead)
  (when *has-started* (for-each draw-player-status! (remov3  *local-client-id* (get-player-ids)))))

(define (tic)
  ; Draw the environment
  (+pano "http://s3.amazonaws.com/ovr-360/photos/OTOY_May/16055_9C31448B-E5C2-4269-BC98-49B1F4A8D5B3_pano.jpg")

  ; Start server if it hasn't started yet.
  (if social? (tic-connected) (draw-share-server-button!)))

; We need to define an init function to guarantee that the message
; containing the results of all top-level code executed during
; startup get processed before the first (tic) call.
(define (init init-parms)
  ;(reset-game!)
  (display (format "(init ~a)\n" init-parms)))

; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when NetHmd is run.
; The init function is optional, use #f if not defined.
(remote "172.24.251.163" #f tic server-parms)