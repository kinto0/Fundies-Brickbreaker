;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 6 Part 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|--- CONSTANTS ---|#
(define WIDTH 200)
(define HEIGHT 200)
(define BG (empty-scene WIDTH HEIGHT))

(define BALL-COLOR "blue")
(define BALL-RADIUS 6)
(define BALL-SPEED 4)
(define THE-BALL (circle BALL-RADIUS "solid" BALL-COLOR))

(define BRICK-COLOR 'red)
(define BRICK-WIDTH 30)
(define BRICK-HEIGHT 10)
(define BRICK-PADDING 10)
(define ROWS 3)
(define COLUMNS 5)
(define THE-BRICK (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR))

(define PADDLE-COLOR "purple")
(define PADDLE-WIDTH 40)
(define PADDLE-HEIGHT BRICK-HEIGHT)
(define PADDLE-Y 190)
(define PADDLE-SPEED 5)
(define THE-PADDLE (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR))



#|--- DATA DEFINITIONS ---|#

;;A Ball is a (make-ball Number Number Number Number)
(define-struct ball (x y vx vy))

; - where the first Number is the ball's x-coordinate
; - the second Number is the ball's y-coordinate
; - the third Number is the ball's x-velocity
; - the fourth Number is the ball's y-velocity

;; Ball -> ???
(define (ball-temp b)
  ( ... (ball-x b) ... (ball-y b) ... (ball-vx b) ... (ball-vy b) ... ))

;; Initial ball for world
(define INITIAL-BALL (make-ball (/ WIDTH 2)
                                (- HEIGHT (* 2 PADDLE-HEIGHT) (/ BALL-RADIUS 2))
                                BALL-SPEED
                                0))

;; Ball for no collision
(define BALL-nc (make-ball 100 150 1 1))

;; Balls for wall collisions
(define BALL-lwc (make-ball 0 150 1 1))
(define BALL-rwc (make-ball 200 150 1 1))
(define BALL-twc (make-ball 150 0 1 1))
(define BALL-bwc (make-ball 150 200 1 1))

;; Balls for brick collisions
(define BALL-lbc (make-ball 85 30 1 1))
(define BALL-rbc (make-ball 115 30 1 1))
(define BALL-bbc (make-ball 100 35 1 1))
(define BALL-tbc (make-ball 100 25 1 1))

;; Balls for paddle collisions
(define BALL-lpc (make-ball 80 (- PADDLE-Y (/ PADDLE-HEIGHT 2)) 1 1))
(define BALL-mpc (make-ball 100 (- PADDLE-Y (/ PADDLE-HEIGHT 2)) 1 1))
(define BALL-rpc (make-ball 120 (- PADDLE-Y (/ PADDLE-HEIGHT 2)) 1 1))

;; A Paddle is a (make-paddle Number)
(define-struct paddle (x))

; - Where x is the x position of paddle

(define (paddle-temp p)
  ( ... (paddle-x p) ... ))

(define PADDLE1 (make-paddle 100))

;; A Brick is a (make-brick Number Number Number)
(define-struct brick (num x y))

; - where the first Number is the brick's health
; - the second number is the brick's x coordinate
; - the third number is the brick's y coordinate

(define (brick-temp b)
  ( ... (brick-x b) ... (brick-y b) ... ))

(define BRICK-EX (make-brick 2 100 30))

;; A List-of-Bricks is one of:
; - '()
; - (cons Brick List-of-Bricks)

(define (lob-temp lob)
  (cond
    [(empty? lob) ... ]
    [(cons? lob) ... ]))

(define INITIAL-BRICKS (list (make-brick 2 20 10)
                             (make-brick 2 60 10)
                             (make-brick 2 100 10)
                             (make-brick 2 140 10)
                             (make-brick 2 180 10)
                             (make-brick 2 20 30)
                             (make-brick 2 60 30)
                             (make-brick 2 100 30)
                             (make-brick 2 140 30)
                             (make-brick 2 180 30)
                             (make-brick 2 20 50)
                             (make-brick 2 60 50)
                             (make-brick 2 100 50)
                             (make-brick 2 140 50)
                             (make-brick 2 180 50)))

;; A World is a (make-world Ball Paddle List-of-Bricks Boolean)
(define-struct world (ball paddle lob launched))

;; Where the:
;; - ball is the ball on the screen
;; - the paddle is the paddle
;; - the lob is the list of bricks
;; - the boolean is whether the ball has been launched yet

(define (world-temp w)
  ( ... (ball-temp (world-ball w)) ...
        (paddle-temp (world-paddle w)) ...
        (lob-temp (world-lob w)) ...
        (world-launched w) ...
        (world-lives w) ...
        (world-score w) ... ) ... )

(define WORLD0 (make-world INITIAL-BALL PADDLE1 INITIAL-BRICKS #false))

(define WORLD-nc (make-world BALL-nc PADDLE1 INITIAL-BRICKS #true))

(define WORLD-lwc (make-world BALL-lwc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-rwc (make-world BALL-rwc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-twc (make-world BALL-twc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-bwc (make-world BALL-bwc PADDLE1 INITIAL-BRICKS #true))

(define WORLD-lbc (make-world BALL-lbc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-rbc (make-world BALL-rbc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-tbc (make-world BALL-tbc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-bbc (make-world BALL-bbc PADDLE1 INITIAL-BRICKS #true))

(define WORLD-lpc (make-world BALL-lpc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-mpc (make-world BALL-mpc PADDLE1 INITIAL-BRICKS #true))
(define WORLD-rpc (make-world BALL-rpc PADDLE1 INITIAL-BRICKS #true))



(define (main _)
  (big-bang WORLD0
    [to-draw draw-world]
    [on-key move-paddle]
    [on-tick tick-world]
    ;[stop-when dead? show-end]
    ))

#|--- FUNCTIONS ---|#

;; speed: Ball -> Number
;; compute the speed of the ball
(check-expect (speed INITIAL-BALL) 4)

(define (speed ball)
  (sqrt (+ (sqr (ball-vx ball))
           (sqr (ball-vy ball)))))

;; new-x-velocity : Ball Number -> Number
;; Produces the new x velocity of a ball that launched off a paddle with this x-coordinate
(check-expect (new-x-velocity INITIAL-BALL 100) 0)
(check-expect (new-x-velocity (make-ball 60 PADDLE-Y 3 4) 100)
              (inexact->exact (* 4.75 -40/26)))

(define (new-x-velocity ball x)
  (inexact->exact
   (* .95
      (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS))
      (speed ball))))

;; new-y-velocity : Ball Number -> Number
;; Produces the new y velocity of a ball that launched off a paddle with this x-coordinate
(check-expect (new-y-velocity INITIAL-BALL 100) -4)
(check-expect (new-y-velocity (make-ball 60 PADDLE-Y 3 4) 100)
              (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))

(define (new-y-velocity ball x)
  (inexact->exact
   (* (- (sqrt (- 1 (sqr (* .95
                            (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS)))))))
      (speed ball))))

;; launch-ball : Ball Number -> Ball
;; Launch ball off paddle with this x-coordinate
(check-expect (launch-ball INITIAL-BALL 100)
              (make-ball 100 173 0 -4))
(check-expect (launch-ball (make-ball 60 PADDLE-Y 3 4) 100)
              (make-ball (+ 60 (inexact->exact (* 4.75 -40/26)))
                         (+ PADDLE-Y (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))
                         (inexact->exact (* 4.75 -40/26))
                         (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26))))))))

(define (launch-ball ball x)
  (make-ball (+ (ball-x ball) (new-x-velocity ball x))
             (+ (ball-y ball) (new-y-velocity ball x))
             (new-x-velocity ball x) (new-y-velocity ball x)))


;; launch-ball-world : World -> World
;; Creates a world in which the ball is launched
(check-expect (launch-ball-world WORLD0)
              (make-world (launch-ball (world-ball WORLD0) (paddle-x (world-paddle WORLD0)))
                          (world-paddle WORLD0)
                          (world-lob WORLD0)
                          #true))

(define (launch-ball-world w)
  (make-world (launch-ball (world-ball w) (paddle-x (world-paddle w)))
                         (world-paddle w)
                         (world-lob w)
                         #true))

              
;; Draw-world : World -> Image
;; Renders the world onto an image
(check-expect (draw-world WORLD0)
              (draw-ball INITIAL-BALL (draw-bricks INITIAL-BRICKS (draw-paddle PADDLE1))))

(define (draw-world w)
  (draw-ball (world-ball w) (draw-bricks (world-lob w) (draw-paddle (world-paddle w)))))

;; Draw-ball : Ball, Image -> Image
;; draws ball over current image
(check-expect (draw-ball INITIAL-BALL BG)
              (place-image THE-BALL (ball-x INITIAL-BALL) (ball-y INITIAL-BALL) BG))

(define (draw-ball ball img)
  (place-image THE-BALL (ball-x ball) (ball-y ball) img))

;; Draw-bricks : LoB, Image -> Image
;; draws bricks over current image
(check-expect (draw-bricks '() BG) BG)
(check-expect (draw-bricks (list (make-brick 2 0 0)) BG) (place-image THE-BRICK 0 0 BG))

(define (draw-bricks bricks img)
  (cond
    [(empty? bricks) (place-image empty-image 0 0 img)]
    [(cons? bricks) (place-image THE-BRICK
                                 (brick-x (first bricks))
                                 (brick-y (first bricks))
                                 (draw-bricks (rest bricks) img))]))
;; draw-paddle : Paddle -> Image
;; draws the paddle on an image
(check-expect (draw-paddle PADDLE1) (place-image THE-PADDLE (paddle-x PADDLE1) PADDLE-Y BG))

(define (draw-paddle paddle)
  (place-image THE-PADDLE (paddle-x paddle) PADDLE-Y BG))

;; move-paddle : World, KeyEvent -> World
;; Moves the paddle on user keypress
(check-expect (move-paddle WORLD0 "right") (move-right WORLD0))
(check-expect (move-paddle WORLD0 "left") (move-left WORLD0))
(check-expect (move-paddle WORLD0 "up") WORLD0)

(define (move-paddle w key)
  (cond
    [(key=? key "left") (move-left w)]
    [(key=? key "right") (move-right w)]
    [(key=? key " ") (launch-ball-world w)]
    [else w]))

;; move-right : World -> World
;; Moves paddle to the right
(check-expect (move-right WORLD0)
              (make-world (world-ball WORLD0)
                          (make-paddle (+ (paddle-x (world-paddle WORLD0)) PADDLE-SPEED))
                          (world-lob WORLD0)
                          (world-launched WORLD0)))

(define (move-right w)
  (cond
    [(>= (+ (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2)) WIDTH) w]
    [(not (world-launched w)) (make-world (make-ball (+ (ball-x (world-ball w)) PADDLE-SPEED)
                                                     (ball-y (world-ball w))
                                                     (ball-vx (world-ball w))
                                                     (ball-vy (world-ball w)))
                                          (make-paddle (+ (paddle-x (world-paddle w)) PADDLE-SPEED))
                                          (world-lob w)
                                          (world-launched w))]
    [else (make-world (world-ball w)
                      (make-paddle (+ (paddle-x (world-paddle w)) PADDLE-SPEED))
                      (world-lob w)
                      (world-launched w))]))


;; move-left : World -> World
;; Moves paddle to the right
(check-expect (move-left WORLD0)
              (make-world (world-ball WORLD0)
                          (make-paddle (- (paddle-x (world-paddle WORLD0)) PADDLE-SPEED))
                          (world-lob WORLD0)
                          (world-launched WORLD0)))

(define (move-left w)
  (cond
    [(<= (- (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2)) 0) w]
    [(not (world-launched w)) (make-world (make-ball (- (ball-x (world-ball w)) PADDLE-SPEED)
                                                     (ball-y (world-ball w))
                                                     (ball-vx (world-ball w))
                                                     (ball-vy (world-ball w)))
                                          (make-paddle (- (paddle-x (world-paddle w)) PADDLE-SPEED))
                                          (world-lob w)
                                          (world-launched w))]
    [else (make-world (world-ball w)
                      (make-paddle (- (paddle-x (world-paddle w)) PADDLE-SPEED))
                      (world-lob w)
                      (world-launched w))]))

;; tick-world : World -> World
;; Checks if the ball has been launched yet
(check-expect (tick-world WORLD0)
              (make-world (launch-ball INITIAL-BALL (/ WIDTH 2)) PADDLE1 INITIAL-BRICKS #true))
(check-expect (tick-world WORLD-nc)
              (move-ball-helper WORLD-nc))

(define (tick-world w)
  (cond
    [(world-launched w) (move-ball-helper w)]
    [(not (world-launched w)) w]))

;; move-ball : World -> World
;; Checks if there is a collision or not and changes ball direction if nececssary
(check-expect (move-ball-helper WORLD-nc) (move-ball WORLD-nc))
(check-expect (move-ball-helper WORLD-lwc) (flip-x WORLD-lwc))
(check-expect (move-ball-helper WORLD-rwc) (flip-x WORLD-rwc))
(check-expect (move-ball-helper WORLD-twc) (flip-y WORLD-twc))

(check-expect (move-ball-helper WORLD-lbc) (flip-x WORLD-lbc))
(check-expect (move-ball-helper WORLD-rbc) (flip-x WORLD-rbc))
(check-expect (move-ball-helper WORLD-tbc) (flip-y WORLD-tbc))
(check-expect (move-ball-helper WORLD-bbc) (flip-y WORLD-bbc))

(check-expect (move-ball-helper WORLD-lpc)
              (launch-ball INITIAL-BALL (paddle-x (world-paddle WORLD-lpc))))
(check-expect (move-ball-helper WORLD-mpc)
              (launch-ball INITIAL-BALL (paddle-x (world-paddle WORLD-mpc))))
(check-expect (move-ball-helper WORLD-rpc)
              (launch-ball INITIAL-BALL (paddle-x (world-paddle WORLD-rpc))))

(define (move-ball-helper w)
  (cond
    [(collision? w)
     (cond
       [(touching-wall? w) (cond
                             [(touching-wall-x? w) (flip-x w)]
                             [(touching-wall-t? w) (flip-y w)])]
       [(brick? (within-brick (world-lob w) (world-ball w))) (cond
                                                               [(within-brick-y? (within-brick (world-lob w) (world-ball w)) (world-ball w)) (remove-brick (flip-y w) (within-brick (world-lob w) (world-ball w)))]
                                                               [(within-brick-x? (within-brick (world-lob w) (world-ball w)) (world-ball w)) (remove-brick (flip-x w) (within-brick (world-lob w) (world-ball w)))]
                                                               [else w])]
       [(touching-paddle? w) (launch-ball-world w)])]
    [else (move-ball w)]))

(define (flip-x w)
  (move-ball (make-world
              (make-ball (ball-x (world-ball w))
                         (ball-y (world-ball w))
                         (* -1 (ball-vx (world-ball w)))
                         (ball-vy (world-ball w)))
              (world-paddle w)
              (world-lob w)
              (world-launched w))))

(define (flip-y w)
  (move-ball (make-world
              (make-ball
               (ball-x (world-ball w))
               (ball-y (world-ball w))
               (ball-vx (world-ball w))
               (* -1 (ball-vy (world-ball w))))
              (world-paddle w)
              (world-lob w)
              (world-launched w))))

(define (remove-brick w brick)
  (make-world (world-ball w)
              (world-paddle w)
              (remove brick (world-lob w))
              (world-launched w)))


;; collision? : World -> Boolean
;; Determines if there is a collision with the ball
(check-expect (collision? WORLD-nc) #false)
(check-expect (collision? WORLD-lwc) #true)

(define (collision? w)
  (or
   (brick? (within-brick (world-lob w) (world-ball w)))
   (touching-wall? w)
   (touching-paddle? w)))

;; move-ball : World -> World
;; Moves the ball
(check-expect (move-ball WORLD-nc)
              (make-world (make-ball 101 151 1 1) PADDLE1 INITIAL-BRICKS #true))

(define (move-ball w)
  (make-world (make-ball (+ (ball-x (world-ball w)) (ball-vx (world-ball w)))
                         (+ (ball-y (world-ball w)) (ball-vy (world-ball w)))
                         (ball-vx (world-ball w))
                         (ball-vy (world-ball w)))
              (world-paddle w)
              (world-lob w)
              (world-launched w)))

;; ----------------- TOUCHING WALL??? ------------------------------------

;; touching-wall : World -> Boolean
;; Determines if the ball is touching a wall
(check-expect (touching-wall? WORLD-lwc) #true)
(check-expect (touching-wall? WORLD-lbc) #false)

(define (touching-wall? w)
  (or
   (touching-wall-x? w)
   (touching-wall-t? w)))

;; touching-wall-z? : World -> Boolean
;; Determines if the ball is touching the right or left wall
(check-expect (touching-wall-x? WORLD-lwc) #false)
(check-expect (touching-wall-x? WORLD-rwc) #true)

(define (touching-wall-x? w)
  (or (>= (+ (ball-x (world-ball w)) BALL-RADIUS) WIDTH)
      (<= (- (ball-x (world-ball w)) BALL-RADIUS) 0)))

;; touching-wall-t? : World -> Boolean
;; Determines if the ball is touching the top wall
(check-expect (touching-wall-t? WORLD-lwc) #false)
(check-expect (touching-wall-t? WORLD-twc) #true)

(define (touching-wall-t? w)
  (<= (- (ball-y (world-ball w)) BALL-RADIUS) 0))

;; touching-wall-b? : World -> Boolean
;; Determines if the ball is touching the bottom wall
(check-expect (touching-wall-b? WORLD-lwc) #false)
(check-expect (touching-wall-b? WORLD-bwc) #true)

(define (touching-wall-b? w)
  (>= (+ (ball-y (world-ball w)) BALL-RADIUS) HEIGHT))

;; ------------------ WITHIN BRICK???????? -------------------------------

;; within-brick? : [List-of Brick] Ball -> Brick
;; Determines if the ball is touching any bricks in the list
(check-expect (within-brick '() BALL-lwc) '())
(check-expect (within-brick INITIAL-BRICKS BALL-lwc) '())
(check-expect (within-brick INITIAL-BRICKS BALL-lbc) BRICK-EX)
(check-expect (within-brick INITIAL-BRICKS BALL-rbc) BRICK-EX)
(check-expect (within-brick INITIAL-BRICKS BALL-tbc) BRICK-EX)
(check-expect (within-brick INITIAL-BRICKS BALL-bbc) BRICK-EX)


(define (within-brick lob ball)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (if (touching-single-brick? (first lob) ball)
                     (first lob)
                     (within-brick (rest lob) ball))]))

;; touching-single-brick : Brick Ball -> Boolean
;; Checks if a ball is touching a single brick
(check-expect (touching-single-brick? BRICK-EX BALL-lwc) #false)
(check-expect (touching-single-brick? BRICK-EX BALL-rwc) #false)
(check-expect (touching-single-brick? BRICK-EX BALL-twc) #false)
(check-expect (touching-single-brick? BRICK-EX BALL-bbc) #true)
(check-expect (touching-single-brick? BRICK-EX BALL-tbc) #true)
(check-expect (touching-single-brick? BRICK-EX BALL-lbc) #true)
(check-expect (touching-single-brick? BRICK-EX BALL-rbc) #true)


(define (touching-single-brick? brick ball)
  (and (within-brick-x? brick ball)
       (within-brick-y? brick ball)))

(define (within-brick-y? brick ball)
  (and (<= (- (brick-y brick) (/ BRICK-HEIGHT 2)) (+ (ball-y ball) BALL-RADIUS))
       (>= (+ (brick-y brick) (/ BRICK-HEIGHT 2)) (- (ball-y ball) BALL-RADIUS))))

(define (within-brick-x? brick ball)
  (and (>= (+ (brick-x brick) (/ BRICK-WIDTH 2)) (- (ball-x ball) BALL-RADIUS))
       (<= (- (brick-x brick) (/ BRICK-WIDTH 2)) (+ (ball-x ball) BALL-RADIUS))))

;; ------------------ TOUCHING PADDLE??????? -------------------------------
(check-expect (touching-paddle? WORLD-lpc) #true)
(check-expect (touching-paddle? WORLD-mpc) #true)
(check-expect (touching-paddle? WORLD-rpc) #true)
(check-expect (touching-paddle? WORLD-twc) #false)

(define (touching-paddle? w)
  (and (= (ball-y (world-ball w)) (- PADDLE-Y (/ PADDLE-HEIGHT 2)))
       (<= (- (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2))
           (ball-x (world-ball w))
           (+ (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2)))))


(main 0)