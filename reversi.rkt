#lang racket/base
(require "remote.rkt")
; Export the identifiers needed for simultanious social development.
(provide init tic server-parms)

; Everything up to this mark will be stripped and replaced
; for the embedded version.
; %%%END-OF-HEADER%%%
;----------------------------------------------------------------------------------

(uri WAV-ACTIVATE    "http://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/ui_object_activate_01.wav")
; TODO: get a wav for this
(uri WAV-BAD-MOVE    "https://s3.amazonaws.com/o.oculuscdn.com/netasset/wav/bad_move.wav")

; To allow social parameters to be set up the same way for both user-invoked and
; remote development invoked, the parameters are defined here, and passed in
; both places.
; ( max-players shutdown-on-empty title icon )
(define server-parms '(4 #t "Reversi" "https://s3.amazonaws.com/o.oculuscdn.com/v/test/social/reversi.jpg"))


;--------------- multi player -----------------

; Support two players and two spectators.
;
; The server state is:
; #( move-number board-list )
;
; The seats are:
; ( black-client-id white-client-id spec1-id spec2-id  0 0 0 0 )
;
; The client state is:
; #( move-number x y )
;
; Server move-number starts at 2 and monotonically increases. It is not reset when
; the board is cleared, otherwise the first move could be unintentionally repeated.
;
; Client move-numebr starts at 0.  If the server move-number is even, black can move.
; If odd, white can move.  A move is selected by setting the client move-number to the
; server move-number and setting x and y.
;
; When the server sees the current black/white client has a move-number equal to the
; server move-number, the move is applied and it changes to the next move-number.
;
; The server controls who is in the white and black chairs and each spectator spot.
;
; This default server state will be set during load and returned by the (init) function,
; so there should always be valid state for (tic).
;
; Social script processing does not start until the first server state message is received from
; the packet server.  This will always include a server-state, which always replaces the
; server-state returned by init.
;
; Should there be a way to run a script as social without starting out single user?
; No, we wouldn't have a way to give the packet server an initial server-state.
;
; The first time a client runs a script tic will define their client-state.  The controlling
; client cannot directly change any client-state but their own, they can only signal in
; server-state that clients should make their own changes.
;
; Client-state is information that all other clients can use to render the player.
;
; When a script is initially started for social applications there will only be a single
; client, the controlling one.
;
; Default seat assignment is handled by the system, but the controlling client can swap
; seats for any reason.
;
; Never acknowledge the existence of clients that the controlling-client has not acknowledged
; the existence of.
;
; Client asks the master server to join a server.
; Master server adds the client to the server's client list and responds with a packet server
; address and apk to launch.
; Client apk starts up HeadNet and waits for a server message from the packet server.
; If server message says they are the controlling client, (init) and (tic) scripts can be started.
; If server message says they are not the controlling client, they must wait until their client-id
; is recognized by the controlling client.
; A client will be recognized by the controlling client when it shows up in the seat array.

; This needs to be valid for reset-game! to work.
(set-server-state! (vector 2 (make-vector 64 0)))

(define (ss-black-id)  (seat-get 0))
(define (ss-white-id)  (seat-get 1))
(define (ss-spec1-id)  (seat-get 2))
(define (ss-spec2-id)  (seat-get 3))

(define (ss-move-number)  (vector-ref *server-state* 0))
(define (ss-board-vector) (vector-ref *server-state* 1))

; Only the server can perform these actions, but the results will be
; propagated to all clients.
(define (ss-move-number! move-num) (vector-set! *server-state* 0 move-num))
(define (ss-board-vector! vec)     (vector-set! *server-state* 1 vec))

(define (set-black-id! cid)  (seat-set! 0 cid))
(define (set-white-id! cid)  (seat-set! 1 cid))
(define (set-spec1-id! cid)  (seat-set! 2 cid))
(define (set-spec2-id! cid)  (seat-set! 3 cid))

;--------------------------------------------------------

; This default value will always be applied before the first (frame),
; then it can be modified at will, and it will be communicated to the
; other clients.
(set-client-state! (vector 0 0 0))

(define (cs-move-number cl)
  (vector-ref (client-state cl) 0))

(define (cs-move-x cl)
  (vector-ref (client-state cl) 1))

(define (cs-move-y cl)
  (vector-ref (client-state cl) 2))

; If (ss-move-number) == *last-move-number* + 1, we can
; animate to the current server-state.  If it is more
; divergent, the server state should be accepted without
; change.
(define *last-move-number* 0)

; When a client chooses a move, set the client state
; so the server will apply it.
(define (set-move! x y)
  (set-client-state! (vector (ss-move-number) x y)))


;--------------------------------------------------------

; The board will hold these values, or 0 for an empty spot.
(define player-black 1)
(define player-white 2)
(define (current-player)
  (+ 1 (remainder (ss-move-number) 2)))

; It will never be a spectator's turn.
; Note that a single client-id can play both sides.
(define (my-turn?)
  (define move (remainder (ss-move-number) 2))
  (or
   (and (= 0 move)
        (= (ss-black-id) *local-client-id*))
   (and (= 1 move)
        (= (ss-white-id) *local-client-id*))))

(define (reset-game!)
  (clear-board!)
  ; The server move number must be bumped to an even number, but
  ; should stay monotonically increasing.
  (set-server-state! (vector (+ (ss-move-number) (remainder (ss-move-number) 2)) *board*))
  (printf "reset server state: ~a\n" *server-state* ))

; If the player who's turn it is has set a move, apply it and
; go to the next move.
(define (apply-new-moves!)
  (define black-client (client-by-id (ss-black-id)))
  (define white-client (client-by-id (ss-white-id)))
  (define move (remainder (ss-move-number) 2))
  (define (move-client cl pl)
    (place (cs-move-x cl) (cs-move-y cl) pl)
    (ss-move-number! (+ 1 (ss-move-number))))
  (cond
    ((and (= move 0) black-client (= (cs-move-number black-client) (ss-move-number)))
     (move-client black-client player-black))
    ((and (= move 1) white-client (= (cs-move-number white-client) (ss-move-number)))
     (move-client white-client player-white))))

;--------------- reversi rules -----------------

; board value 0 = empty, 1 = black, 2 = white
(define *board* (make-vector 64 0))

(define (board-get x y)
  (vector-ref *board* (+ (* y 8) x)))

(define (board-set! x y c)
  (vector-set! *board* (+ (* y 8) x) c))

; Debug printing tool
(define (board)
  (define (row y)
    (for-each (lambda (x) (display (board-get x y))) (iota 8))
    (newline))
  (for-each row (iota 8)))

(define (clear-board!)
  (set! *board* (make-vector 64 0))
  (board-set! 3 3 1)
  (board-set! 4 4 1)
  (board-set! 3 4 2)
  (board-set! 4 3 2))

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
  (scan-start -1 -1)
  ; Put the updated board vector back into the server-state list
  (ss-board-vector! *board*))

; A move is only legal if it is to an empty space and it
; would result in at least one flip.
(define (legal? x y c)
  (define take (if (= c 1) 2 1)) ; the color we can capture
  (define (scan-start dx dy)
    (define (flip fx fy)
      (cond
        ((and (= fx x) (= fy y))  #f) ; back to the start point
        (else #t)))                   ; an actual capture, the move is legal
    (define (scan sx sy)      
      (cond
        ((not (and (< -1 sx 8) (< -1 sy 8))) #f) ; off the edge, didn't find a match
        ((= c (board-get sx sy)) (flip (- sx dx) (- sy dy))) ; we can take this, continue checking
        ((not (= take (board-get sx sy))) #f) ; hit a spot that isn't our take color
        (else (scan (+ sx dx) (+ sy dy)))))   ; this may be flipable, keep looking
    (scan (+ x dx) (+ y dy)))
  (cond
    ((> (board-get x y) 0) #f) ; something else already there
    (#t (or (scan-start 1 0)
            (scan-start 0 1)
            (scan-start -1 0)
            (scan-start 0 -1)
            (scan-start 1 1)
            (scan-start 1 -1)
            (scan-start -1 1)
            (scan-start -1 -1)))))
    
;--------------- Computer play ----------------------

(define *computer-state* 0)

(define (computer-move player)
  (define x (quotient *computer-state* 8))
  (define y (remainder *computer-state* 8))
  (set! *computer-state* (+ 1 *computer-state*))
  (place x y player))        

;--------------- Procedural Board shaders ----------------------

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


(define board-vertex-shader
"#version 300 es
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

(+shader "board" board-vertex-shader
             
             "#version 300 es
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
")

(+shader "disc-white" board-vertex-shader
             "#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out lowp vec4 out_FragColor;
void main()
{
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
highp int isSet;
if ( y < 2) isSet = int(UniformColor.x);
else if ( y < 4) isSet = int(UniformColor.y);
else if ( y < 6) isSet = int(UniformColor.z);
else isSet = int(UniformColor.w);
y &= 3;
int bit = (1 << ((y<<3) + x));
mediump vec2 p = fract( oTexCoord * 8.0 ) - 0.5;
mediump float dist = length( p );
mediump float on = (dist < 0.4) ? 1.0 : 0.0;
if ( ((isSet & bit) > 0) && dist < 0.4 )
  out_FragColor = vec4( 1.0, 1.0, 1.0, 1.0 );
else
  out_FragColor = vec4( 0.0, 0.0, 0.0, 0.0 );
}
")

(+shader "disc-black" board-vertex-shader
             "#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out lowp vec4 out_FragColor;
void main()
{
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
highp int isSet;
if ( y < 2) isSet = int(UniformColor.x);
else if ( y < 4) isSet = int(UniformColor.y);
else if ( y < 6) isSet = int(UniformColor.z);
else isSet = int(UniformColor.w);
y &= 3;
int bit = (1 << ((y<<3) + x));
mediump vec2 p = fract( oTexCoord * 8.0 ) - 0.5;
mediump float dist = length( p );
mediump float on = (dist < 0.4) ? 1.0 : 0.0;
if ( ((isSet & bit) > 0) && dist < 0.4 )
  out_FragColor = vec4( 0.0, 0.0, 0.0, 1.0 );
else
  out_FragColor = vec4( 0.0, 0.0, 0.0, 0.0 );
}
")


(+shader "cursor" board-vertex-shader
             "#version 300 es
in highp vec2 oTexCoord;
uniform highp vec4 UniformColor;
out lowp vec4 out_FragColor;
void main()
{
mediump float d = length(oTexCoord - UniformColor.xy);
int x = int( oTexCoord.x * 8.0 );
int y = int( oTexCoord.y * 8.0 );
int c = ( x ^ y ) & 1;
if ( d < 0.02 )
  out_FragColor = vec4( UniformColor.z, UniformColor.z, UniformColor.z, 1.0);
else
  out_FragColor = vec4( 0.0, 0.0, 0.0, 0.0);
}
")

;----------------------------------------------------------

(define *client-yaw* 0.0)

;-----------------
; text-button
;
; Position it 45 degree to the right of the primary view direction
;-----------------
(define (text-button title height)
  (define bounds-trans (mat4-compose (mat4-translate -0.5 -0.3 -0.5) 
                                     (mat4-scale/xyz 1.0 0.15 0.15) 
                                     (mat4-translate 0.0 height -2.0) 
                                     (mat4-rotate-y (- *client-yaw* (* 0.25 pi)))))
  (define gaze-now (gaze-on-bounds? bounds3-unit bounds-trans))
  
  ; Position the text
  (+text title
             (mat4-compose 
              (mat4-scale 2.0) 
              (mat4-translate 0.0 height -2.0)
              (mat4-rotate-y (- *client-yaw* (* 0.25 pi))))
             (if gaze-now 
                 (opt-parm 1.0 1.0 0.5 1.0) 
                 (opt-parm 0.5 0.5 1.0 1.0)))
  
  ; if an input click just happened and we are gazing on it, change rooms
  (if (and (pressed-action) gaze-now)
      (begin
        (+sound WAV-ACTIVATE)
        #t)
      #f)
  )

;----------------------------------------------------------

; If one of the main player slots is open,
; take a spectator if available.
(define (choose-players!)
  (when (= 0 (ss-black-id))
    (if (= 0 (ss-spec1-id))
        (swap-seats! 0 3)
        (swap-seats! 0 2)))
  (when (= 0 (ss-white-id))
    (if (= 0 (ss-spec1-id))
        (swap-seats! 1 3)
        (swap-seats! 1 2))))

; Swap the black and white players.
(define (swap-sides!)
  (swap-seats! 0 1))
  
; Swaps the white player for the next higher client-id that isn't
; the white player.
(define (change-player!)
    (if (or
         (> (ss-spec1-id) (ss-white-id) (ss-spec2-id))
         (> (ss-spec2-id) (ss-spec1-id) (ss-white-id))
         (> (ss-white-id) (ss-spec2-id) (ss-spec1-id)))
        (swap-seats! 1 2)
        (swap-seats! 1 3)))

;----------------------------------------------------------

; Board location centered at the origin, some distance below 0 y
(define board-xform (mat4-compose (mat4-translate -0.5 -0.5 0.0)
                                  (mat4-scale 0.5)
                                  (mat4-rotate-x pi/2)
                                  (mat4-translate 0.0 -0.5 0.0) ))
(define black-edge-xform (mat4-compose (mat4-translate -0.5 -0.5 0.0)
                                  (mat4-scale/xyz 0.5 0.125 0.0)
                                  (mat4-rotate-x pi/2)
                                  (mat4-translate 0.0 -0.5 0.3125) ))
(define white-edge-xform (mat4-compose (mat4-translate -0.5 -0.5 0.0)
                                  (mat4-scale/xyz 0.5 0.125 0.0)
                                  (mat4-rotate-x pi/2)
                                  (mat4-translate 0.0 -0.5 -0.3125) ))
(define black-text-xform (mat4-compose
                                  (mat4-rotate-x -pi/2)
                                  (mat4-translate 0.0 -0.5 0.33) ))
(define white-text-xform (mat4-compose
                                  (mat4-rotate-x -pi/2)
                                  (mat4-rotate-y pi)
                                  (mat4-translate 0.0 -0.5 -0.33) ))

; The black and white players have fixed positions.
; The remaining players are sorted by client id number into
; the spectator spots.
(define (set-position)
  (cond
    ((= *local-client-id* (ss-black-id))
     (set! *client-yaw* 0.0)
     (+set-position (make-vec3 0.0 0.0 0.9) *client-yaw*))
    ((= *local-client-id* (ss-white-id))
     (set! *client-yaw* pi)
     (+set-position (make-vec3 0.0 0.0 -0.9) *client-yaw*))
    ((= *local-client-id* (ss-spec1-id))
     (set! *client-yaw* pi*3/2)
     (+set-position (make-vec3 -0.9 0.0 0.0) *client-yaw*))
    ((= *local-client-id* (ss-spec2-id))
     (set! *client-yaw* (- pi*3/2))
     (+set-position (make-vec3 -0.9 0.0 0.0) *client-yaw*))
    (#t (printf "Client-id ~a not in seat list\n" *local-client-id*))))


;-----------------
; tic
;
;-----------------
(define (tic)
;  (printf "tic start server state: ~a\n" *server-state* )
  
  ; Only perform server-state related tasks on the controlling client.
  (when (controlling-client?)
     ; If the current player has decided on a move, apply it
     ; and switch to the other player.
     (apply-new-moves!)
     
     ; Draw the UI buttons off to the side
     (when (and (not social?)
                (text-button "Share Server" 0.25))
       (apply +share-server server-parms))

    (when (text-button "Swap Sides" 0.0)
      (swap-sides!))

    ; only allow change-player if more than 2 clients
    (when (and (> (length *clients*) 2)
               (text-button "Change Player" 0.25))
      (change-player!))

    ; Restart game valid at any time
    (when (text-button "Restart Game" -0.25)
      (reset-game!))

    ; Reorganize the players if necessary.
    (choose-players!))
    
  ; Set our view/avatar position based on where our client-id is in the server state.
  (set-position)
  
  ; Find the spot on the board the local client is looking at.
  (define spot (intersect-line-texture (mat4-origin *pose-inverse*) (mat4-forward *pose-inverse*) board-xform))
  
  ; Reference the board out of server-state.
  (set! *board* (ss-board-vector))
  
  ; Allow a play command to be set for this client if it is our turn
  ; and we have clicked on the board.
  ; We don't actually apply the move, just set it in our client-state,
  ; so the controlling-client (which may not be us) can apply it
  ; next tic.
  (when (and (my-turn?)
             (pressed-action)
             (< 0.0 (vec3-x spot) 1.0)
             (< 0.0 (vec3-y spot) 1.0))
    (let ( (x (floor->exact (* 8 (vec3-x spot))))
           (y (floor->exact (* 8 (vec3-y spot)))) )
      (if (legal? x y (current-player))
          (set-move! x y)
          (+sound WAV-BAD-MOVE))))

  ; If a move was just made in the server state, play a sound
  (when (= (+ 1 *last-move-number*) (ss-move-number))
    (+sound WAV-ACTIVATE))
  (set! *last-move-number* (ss-move-number))
  
  ; draw the environment
  (+pano "http://s3.amazonaws.com/o.oculuscdn.com/v/test/social/avatars/office_demo.JPG")

  ; draw the move-cursor on the board if it is the local client's turn
  (+quad "_white" 
             board-xform
             'depth-mask
             (opt-shader "board"))
  (+quad "_white" 
             board-xform
             'depth-mask
             (opt-parmv (board->vec4 player-white))
             (opt-shader "disc-white"))
  (+quad "_white" 
             board-xform
             'depth-mask
             (opt-parmv (board->vec4 player-black))
             (opt-shader "disc-black"))
  (when (my-turn?)
    (+quad "_white" 
               board-xform
               'depth-mask
               (opt-parm (vec3-x spot) 
                         (vec3-y spot) 
                         (if (= (current-player) player-black) 0.2 0.8)
                         0.0)
               (opt-shader "cursor")))

  ; black edge
  (+quad "_white" 
             black-edge-xform
             'depth-mask
             (opt-parm 0.0 0.0 0.0 1.0))
  ; white edge
  (+quad "_white" 
             white-edge-xform
             'depth-mask)

  ; Piece counts
  (define (count-pieces p)
    (define board (ss-board-vector))
    (define (cv x total)
      (cond
        ((= x 64) total)
        ((= (vector-ref board x) p) (cv (+ 1 x) (+ 1 total)))
        (#t (cv (+ 1 x) total))))
    (cv 0 0))
  (+text (format "White:~a Black:~a" (count-pieces player-white) (count-pieces player-black))
         (mat4-compose (mat4-rotate-y -pi/2) (mat4-translate 0.5 -0.3 0.0)))
  
  (+text "Your Move" 
             (if (= 0 (remainder (ss-move-number) 2))
                 black-text-xform
                 white-text-xform)
             (opt-parm 0.5 0.5 0.5 1.0))

  )

; We need to define an init function to guarantee that the message
; containing the results of all top-level code executed during
; startup get processed before the first (tic) call.
(define (init init-parms)
  (set-black-id! *local-client-id*)
  (reset-game!)
  (display (format "(init ~a)\n" init-parms)))

; This connects to the HMD over TCP when run from DrRacket, and is ignored when embedded.
; Replace the IP address with the value shown on the phone when vrscript is run.
;
; If there is an optional server-parms parameter after tic, VrScript will
; attempt to connect to that social server, starting it if it doesn't exist.
;
; Other users can connect via social or an explicit intent, but they will be using the
; script downloaded from the net, which may be different than what is being run over TCP.
;
; IMPORTANT: *server-state* must have been initialized to something
; valid by top level function calls before remote is invoked, so it can be 
(reset-game!)

; To allow running multiple remote social instances at once, comment out
; the normal (remote ...) call, then create a separate file for each target phone
; with the following lines:
;
;#lang racket/base
;(require "reversi.rkt")
;(require "remote.rkt")
;(remote "172.22.52.41" init tic server-parms)
;(remote "172.22.52.94" init tic server-parms)


(remote "172.22.52.94" init tic #;server-parms)
