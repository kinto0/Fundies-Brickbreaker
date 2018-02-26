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
(define BALL-lpc (make-ball 85 (+ (/ PADDLE-HEIGHT 2) PADDLE-Y) 1 1))
(define BALL-mpc (make-ball 100 (+ (/ PADDLE-HEIGHT 2) PADDLE-Y) 1 1))
(define BALL-rpc (make-ball 115 (+ (/ PADDLE-HEIGHT 2) PADDLE-Y) 1 1))

;; A Paddle is a (make-paddle Number)

(define-struct paddle (x))
(define PADDLE1 (make-paddle 100))

;; A Brick is a (make-brick Number Number Number)

(define-struct brick (num x y))

; - where the first Number is the brick's health
; - the second number is the brick's x coordinate
; - the third number is the brick's y coordinate
(define BRICK-EX (make-brick 2 100 30))

;; A List-of-Bricks is one of:
; - '()
; - (cons Brick List-of-Bricks)

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
;; Interp: __DO THIS LATER PLEASE_________________________________________________________________________
(define-struct world (ball paddle lob launched))

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
;; speed: Ball -> Number
;; compute the speed of the ball
(check-expect (speed INITIAL-BALL) 4)
(define (speed ball)
  (sqrt (+ (sqr (ball-vx ball))
           (sqr (ball-vy ball)))))

;;new-x-velocity : Ball Number -> Number
;;Produces the new x velocity of a ball that launched off a paddle with this x-coordinate
(define (new-x-velocity ball x)
  (inexact->exact
   (* .95
      (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS))
      (speed ball))))
(check-expect (new-x-velocity INITIAL-BALL 100) 0)
(check-expect (new-x-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* 4.75 -40/26)))

;;new-y-velocity : Ball Number -> Number
;;Produces the new y velocity of a ball that launched off a paddle with this x-coordinate
(define (new-y-velocity ball x)
  (inexact->exact
   (* (- (sqrt (- 1 (sqr (* .95
                            (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS)))))))
      (speed ball))))
(check-expect (new-y-velocity INITIAL-BALL 100) -4)
(check-expect (new-y-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))

;;launch-ball : Ball Number -> Ball
;;Launch ball off paddle with this x-coordinate
(define (launch-ball ball x)
  (make-ball (+ (ball-x ball) (new-x-velocity ball x))
             (+ (ball-y ball) (new-y-velocity ball x))
             (new-x-velocity ball x) (new-y-velocity ball x)))
(check-expect (launch-ball INITIAL-BALL 100)
              (make-ball 100 173 0 -4))
(check-expect (launch-ball (make-ball 60 190 3 4) 100)
              (make-ball (+ 60 (inexact->exact (* 4.75 -40/26)))
                         (+ 190 (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))
                         (inexact->exact (* 4.75 -40/26))
                         (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26))))))))

;; Draw-world : World -> Image
;; Renders the world onto an image
(check-expect (draw-world WORLD0)
              (draw-ball INITIAL-BALL (draw-bricks INITIAL-BRICKS (draw-paddle PADDLE1))))

(define (draw-world w)
  (draw-ball (world-ball w) (draw-bricks INITIAL-BRICKS (draw-paddle (world-paddle w)))))

;; Draw-ball : Ball, Image -> Image
;; draws ball over current image
(check-expect (draw-ball INITIAL-BALL BG) (place-image THE-BALL (ball-x INITIAL-BALL) (ball-y INITIAL-BALL) BG))

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
(check-expect (draw-paddle PADDLE1) (place-image THE-PADDLE (paddle-x PADDLE1) 190 BG))

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
    [else w]))

;; move-right : World -> World
;; Moves paddle to the right
(check-expect (move-right WORLD0)
              (make-world (world-ball WORLD0)
                          (make-paddle (+ (paddle-x (world-paddle WORLD0)) PADDLE-SPEED))
                          (world-lob WORLD0)
                          (world-launched WORLD0)))

(define (move-right w)
  (if (< (paddle-x (world-paddle w)) (- WIDTH (/ PADDLE-WIDTH 2)))
      (make-world (world-ball w)
                  (make-paddle (+ (paddle-x (world-paddle w)) PADDLE-SPEED))
                  (world-lob w)
                  (world-launched w))
      w))

;; move-left : World -> World
;; Moves paddle to the right
(check-expect (move-left WORLD0)
              (make-world (world-ball WORLD0)
                          (make-paddle (- (paddle-x (world-paddle WORLD0)) PADDLE-SPEED))
                          (world-lob WORLD0)
                          (world-launched WORLD0)))

(define (move-left w)
  (if (< (/ PADDLE-WIDTH 2) (paddle-x (world-paddle w)))
      (make-world (world-ball w)
                  (make-paddle (- (paddle-x (world-paddle w)) PADDLE-SPEED))
                  (world-lob w)
                  (world-launched w))
      w))

;; tick-world : World -> World
;; Checks if the ball has been launched yet
(check-expect (tick-world WORLD0) (make-world (launch-ball INITIAL-BALL (/ WIDTH 2)) PADDLE1 INITIAL-BRICKS #true))
(check-expect (tick-world WORLD-nc) (move-ball-helper WORLD-nc))

(define (tick-world w)
  (cond
    [(world-launched w) (move-ball-helper w)]
    [(not (world-launched w))
     (make-world (launch-ball INITIAL-BALL (/ WIDTH 2)) (world-paddle w) (world-lob w) #true)]))

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

(check-expect (move-ball-helper WORLD-lpc) (bounce-l WORLD-bbc))
(check-expect (move-ball-helper WORLD-mpc) (flip-y WORLD-bbc))
(check-expect (move-ball-helper WORLD-rpc) (bounce-r WORLD-bbc))


(define (move-ball-helper w)
  (cond
    [(collision? w)
     (cond
       [(touching-wall? w) (cond
                             [(touching-wall-r? w) (flip-x w)]
                             [(touching-wall-l? w) (flip-x w)]
                             [(touching-wall-t? w) (flip-y w)])]
       [(brick? (within-brick (world-lob w) (world-ball w))) (cond
                                                                 [(touching-brick-l? (within-brick (world-lob w) (world-ball w)) (world-ball w)) (flip-x w)]
                                                                 [(touching-brick-r? (within-brick (world-lob w) (world-ball w)) (world-ball w)) (flip-x w)]
                                                                 [(touching-brick-t? (within-brick (world-lob w) (world-ball w)) (world-ball w)) (flip-y w)]
                                                                 [(touching-brick-b? (within-brick (world-lob w) (world-ball w)) (world-ball w)) (flip-y w)]
                                                                 [else w])]
       [(touching-paddle? w) (cond
                               [(touching-paddle-l? w) (bounce-l w)]
                               [(touching-paddle-m? w) (flip-y w)]
                               [(touching-paddle-r? w) (bounce-r w)])])]
    ;;JUST USE LAUNCH BALL FOR ABOVE -GWEN
    [else (move-ball w)]))

(define (flip-x w)
  (move-ball (make-world (make-ball (ball-x (world-ball w)) (ball-y (world-ball w)) (* -1 (ball-vx (world-ball w))) (ball-vy (world-ball w))) (world-paddle w) (world-lob w) (world-launched w))))

(define (flip-y w)
  (move-ball (make-world (make-ball (ball-x (world-ball w)) (ball-y (world-ball w)) (ball-vx (world-ball w)) (* -1 (ball-vy (world-ball w)))) (world-paddle w) (world-lob w) (world-launched w))))

(define (bounce-r w)
  (move-ball (make-world (make-ball (ball-x (world-ball w)) (ball-y (world-ball w)) (+ 20 (ball-vx (world-ball w))) (ball-vy (world-ball w))) (world-paddle w) (world-lob w) (world-launched w))))

(define (bounce-l w)
  (move-ball (make-world (make-ball (ball-x (world-ball w)) (ball-y (world-ball w)) (- 20 (ball-vx (world-ball w))) (ball-vy (world-ball w))) (world-paddle w) (world-lob w) (world-launched w))))

;; collision? : World -> Boolean
;; Determines if there is a collision with the ball
(check-expect (collision? WORLD-nc) #false)
(check-expect (collision? WORLD-lwc) #true)

(define (collision? w)
  (or (brick? (within-brick (world-lob w) (world-ball w))) (touching-wall? w) (touching-paddle? w)))

;; move-ball : World -> World
;; Moves the ball
(check-expect (move-ball WORLD-nc) (make-world (make-ball 101 151 1 1) PADDLE1 INITIAL-BRICKS #true))

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
  (or (touching-wall-r? w) (touching-wall-l? w) (touching-wall-t? w)))

;; touching-wall-r? : World -> Boolean
;; Determines if the ball is touching the right wall
(check-expect (touching-wall-r? WORLD-lwc) #false)
(check-expect (touching-wall-r? WORLD-rwc) #true)

(define (touching-wall-r? w)
  (>= (+ (ball-x (world-ball w)) BALL-RADIUS) WIDTH))

;; touching-wall-l? : World -> Boolean
;; Determines if the ball is touching the left wall
(check-expect (touching-wall-l? WORLD-rwc) #false)
(check-expect (touching-wall-l? WORLD-lwc) #true)

(define (touching-wall-l? w)
  (<= (- (ball-x (world-ball w)) BALL-RADIUS) 0))

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
    [(cons? lob) (if (touching-single-brick? (first lob) ball) (first lob) (within-brick (rest lob) ball))]))

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
  (and (within-brick-t? brick ball) (within-brick-b? brick ball) (within-brick-l? brick ball) (within-brick-r? brick ball)))

;; within-brick-l? : Brick Ball -> Boolean
;; Checks if the ball is within the brick's left boundary
(check-expect (within-brick-l? BRICK-EX BALL-lwc) #false)
(check-expect (within-brick-l? BRICK-EX BALL-lbc) #true)
(check-expect (within-brick-l? BRICK-EX BALL-rbc) #true)


(define (within-brick-l? brick ball)
  (<= (- (brick-x brick) (/ BRICK-WIDTH 2)) (+ (ball-x ball) BALL-RADIUS)))

;; within-brick-r? : Brick Ball -> Boolean
;; Checks if the ball is within the brick's right boundary
(check-expect (within-brick-r? BRICK-EX BALL-rwc) #false)
(check-expect (within-brick-r? BRICK-EX BALL-rbc) #true)

(define (within-brick-r? brick ball)
  (>= (+ (brick-x brick) (/ BRICK-WIDTH 2)) (- (ball-x ball) BALL-RADIUS)))

;; within-brick-b? : Brick Ball -> Boolean
;; Checks if the ball is within the brick's bottom boundary
(check-expect (within-brick-b? BRICK-EX BALL-lwc) #false)
(check-expect (within-brick-b? BRICK-EX BALL-bbc) #true)

(define (within-brick-b? brick ball)
  (>= (+ (brick-y brick) (/ BRICK-HEIGHT 2)) (- (ball-y ball) BALL-RADIUS)))

;; within-brick-t? : Brick Ball -> Boolean
;; Checks if the ball is within the brick's top boundary
(check-expect (within-brick-t? BRICK-EX BALL-twc) #false)
(check-expect (within-brick-t? BRICK-EX BALL-tbc) #true)

(define (within-brick-t? brick ball)
  (<= (- (brick-y brick) (/ BRICK-HEIGHT 2)) (+ (ball-y ball) BALL-RADIUS)))



;; touching-brick-l? : Brick Ball -> Boolean
;; Checks if the ball is touching the brick's left boundary
(check-expect (touching-brick-l? BRICK-EX BALL-lwc) #false)
(check-expect (touching-brick-l? BRICK-EX BALL-lbc) #true)
(check-expect (touching-brick-l? BRICK-EX BALL-rbc) #false)

(define (touching-brick-l? brick ball)
  (= (- (brick-x brick) (/ BRICK-WIDTH 2)) (+ (ball-x ball) BALL-RADIUS)))

;; touching-brick-r? : Brick Ball -> Boolean
;; Checks if the ball is touching the brick's right boundary
(check-expect (touching-brick-r? BRICK-EX BALL-rwc) #false)
(check-expect (touching-brick-r? BRICK-EX BALL-rbc) #true)

(define (touching-brick-r? brick ball)
  (= (+ (brick-x brick) (/ BRICK-WIDTH 2)) (- (ball-x ball) BALL-RADIUS)))

;; touching-brick-b? : Brick Ball -> Boolean
;; Checks if the ball is touching the brick's bottom boundary
(check-expect (touching-brick-b? BRICK-EX BALL-lwc) #false)
(check-expect (touching-brick-b? BRICK-EX BALL-bbc) #true)

(define (touching-brick-b? brick ball)
  (= (+ (brick-y brick) (/ BRICK-HEIGHT 2)) (- (ball-y ball) BALL-RADIUS)))

;; touching-brick-t? : Brick Ball -> Boolean
;; Checks if the ball is touching the brick's top boundary
(check-expect (touching-brick-t? BRICK-EX BALL-twc) #false)
(check-expect (touching-brick-t? BRICK-EX BALL-tbc) #true)

(define (touching-brick-t? brick ball)
  (= (- (brick-y brick) (/ BRICK-HEIGHT 2)) (+ (ball-y ball) BALL-RADIUS)))

;; ------------------ TOUCHING PADDLE??????? -------------------------------

(define (touching-paddle? w)
  (and (touching-paddle-l? w) (touching-paddle-r? w) (touching-paddle-m? w)))

(define (touching-paddle-l? w)
  (and (= (ball-y (world-ball w)) 190)
       (< (- (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2)) (- (ball-x (world-ball w)) BALL-RADIUS) (+ (- (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2)) (/ PADDLE-WIDTH 3)))))
(define (touching-paddle-m? w)
  (and (= (ball-y (world-ball w)) 190)
       (< (+ (- (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2)) (/ PADDLE-WIDTH 3)) (- (ball-x (world-ball w)) BALL-RADIUS) (- (paddle-x (world-paddle w)) (* 2 (/ PADDLE-WIDTH 3))))))
(define (touching-paddle-r? w)
  (and (= (ball-y (world-ball w)) 190)
       (< (- (paddle-x (world-paddle w)) (* 2 (/ PADDLE-WIDTH 3))) (- (ball-x (world-ball w)) BALL-RADIUS) (+ (paddle-x (world-paddle w)) (/ PADDLE-WIDTH 2)))))


(main 0)