;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw05b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
 Number 1 - The difference between syntax and semantics.
 Answer:
  Syntax is the structural rules of a language and semantics is the
    actual uses of the languages and strings we type.

 Number 2 - Define ambiguous
 Answer:
  Ambiguous in programming language terms is simply the fact that you
    can have more than one way to write certain grammars to do the same
    thing.

 Number 3 - show a parse tree for the string a b a a.
  attached file in dropbox for picture of freehand tree

 Number 4 - leftmost derivation of foo( a, b )
 1. stmt -> assignment, subr_call
 3. subr_call -> id ( arg_list )
 8. arg_list -> id ( expr args_tail )
 9a. args_tail -> id ( expr, arg_list )
 8. arg_list -> id ( expr, expr args_tail)
 9b. arg_tail -> id ( expr, expr )
 4. expr -> id ( primary, expr )
 6a. primary -> id ( id, expr )
 4. expr -> id ( id, primary )
 6a. primary -> id ( id, id )

 Number 5 
 <JsFunc> → function <JsIdent>( <JsIdents> ) {
            <JsBody>
          }
 a) <JsBody> → '() | <JsStmt>
    <JsStmt> → <JsStmt> | <JsStmt><JsBody>
 b) x, ; 
 c) <JsIndents> → '() | <JsIndent> | <JsIndent>, <JsIndents>
    <JsIndent> → id
 d) <JsFunc> → function <JsIdent>( <JsIdents> ) {
             <JsBody>
          }
    <JsIdent> → function id ( <JsIdents> ) {
             <JsBody>
          }
    <JsIdents> → function id ( <JsIndent>, <JsIndents> ) {
             <JsBody>
          }
    <JsIdent> → function id ( id, <JsIndents> ) {
             <JsBody>
          }
    <JsIdents> → function id ( id, <JsIndent> ) {
             <JsBody>
          }
    <JsIdent> → function id ( id, id ) {
             <JsBody>
          }
    <JsBody> → function id ( id, id ) {
             <JsStmt>
          }
    <JsStmt> → function id ( id, id ) {
             <JsStmt><JsBody>
          }
    <JsStmt> → function id ( id, id ) {
             <JsStmt><JsStmt>
          }
    => function f ( id, id ) {
            <JsStmt><JsStmt>
          }
    => function f ( a, id ) {
            <JsStmt><JsStmt>
          }
    => function f ( a, b ) {
            <JsStmt><JsStmt>
          }
    => function f ( a, b ) {
            z = a*b; <JsStmt>
          }
    => function f ( a, b ) {
            z = a*b; return a+z;
          }

 Number 6
  <BoolExpr> -> <id> | <id> <BoolOp> | <id> <NumPredicate> | <id> <NumExpr>
  <BoolOp> -> && <BoolExpr> | || <BoolExpr> | ! <BoolExpr> | == <BoolExpr>
  <NumPredicate> -> > <BoolExpr> | < <BoolExpr> | >= <BoolExpr> | <= <BoolExpr> | == <BoolExpr>
  <NumExpr> -> + | - | * | /

 Number 7
 Used : http://www.radford.edu/~itec380/2015fall-ibarland/Homeworks/hw04b-soln.rkt
 - a bullet:
   - x and y coordinate
   - a y-direction (+1 or -1) -- this lets the same object represent both the missiles
     fired
   - since the size/color of a bullet is invariant, no other fields.
     (If you had different size color/bullets for the player and the aliens,
      then you'd have subtypes for each such bullet-type.)
 |#

(define-struct bullet (x y dy))
(define b1 (make-bullet 1 1 1))
(define b2 (make-bullet 2 2 -1))

; Template for bullet processing
; A bullet is :
;  (make-bullet [real] [real] [real])
(define (template-bullet a-bullet)
  (... (bullet-x a-bullet)
   ... (bullet-y a-bullet)
   ... (bullet-dy a-bullet)))

#|
  public Bullet bulletFunction(Bullet b){
    return ...
  }
|#

; Number 8
; Move-bullet : bullet -> bullet
(define (move-bullet a-bullet)
  (make-bullet
   (bullet-x a-bullet)
   (+ (bullet-dy a-bullet) (bullet-y a-bullet))
   (bullet-dy a-bullet)))

(check-expect (move-bullet (make-bullet 1 1 -1)) (make-bullet 1 0 -1))
(check-expect (move-bullet (make-bullet 1 1 1)) (make-bullet 1 2 1))
(check-expect (move-bullet (make-bullet 0 0 1)) (make-bullet 0 1 1))

#|
  public Bullet moveBullet(Bullet aBullet){
     double oldX, oldY, oldDy, newY;
     oldX = aBullet.getX();
     oldY = aBullet.getY();
     oldDy = aBullet.getDy();
     newY = oldY + oldDy;
     return new Bullet(oldX, newY, oldDy);
  }
|#

; Number 9
; An alien is:
;   (make-alien [real] [real])
;   `x`,`y` is the location of the CENTER of the alien, in pixels, from the upper-left corner
;   (usual computer-graphics convention coordinates increasing as you go to the right and down).
(define-struct alien (x y))
(define a1 (make-alien 50 70))
(define a2 (make-alien 60 70))

; move-alien : alien, real, real -> alien
(define (template-alien an-alien)
  (... (alien-x an-alien)
   ... (alien-y an-alien)))


#|  Java:
class Alien {
  double x,y;   // the center of the alien, in pixels from upper-left.

  Alien( double _x, double _y ) {
    this.x = _x;
    this.y = _y;
    }

  void test() {
    new Alien(50,70);
    Alien a0 = new Alien(0,-2);
    }
  }
|#

(define (move-alien an-alien number-x number-y)
  (make-alien
   (+ number-x (alien-x an-alien))
   (+ number-y (alien-y an-alien))))

(check-expect (move-alien (make-alien 50 70) -10 -10 ) (make-alien 40 60))
(check-expect (move-alien (make-alien 50 70) 0 0 ) (make-alien 50 70))
(check-expect (move-alien (make-alien 50 70) 10 10 ) (make-alien 60 80))

#|
  public Alien moveBullet(Alien anAlien, double X, double Y){
     double oldX, oldY, newY, newX;
     oldX = anAlien.getX();
     oldY = anAlien.getY();
     newX = oldX + X;
     newY = oldY + Y;
     return new Alien(newX, newY);
  }
|#

(require 2htdp/universe)

; Number 10
; A cannon is:
;    a real number
(define-struct cannon (number))
(define c1 (make-cannon 20))
(define c2 (make-cannon 50))

(define (cannon-template a-cannon)
  (... (cannon-number a-cannon)))

(define (cannon-handle-key a-cannon a-key)
  (cond
    [(key=? a-key "left")  (make-cannon(- (cannon-number a-cannon) 1))]
    [(key=? a-key "right") (make-cannon(+ 1 (cannon-number a-cannon)))]
    [else (make-cannon (cannon-number a-cannon))])) 

(check-expect (cannon-handle-key (make-cannon 4) "left")
                                 (make-cannon 3))
(check-expect (cannon-handle-key (make-cannon 4) "right")
                                 (make-cannon 5))
(check-expect (cannon-handle-key (make-cannon 4) " ")
                                 (make-cannon 4))

; Number 11
; A world is:
;    a cannon, a bullet, an alien
(define-struct world (cannon bullet alien))
(define w1 (make-world (make-cannon 20) (make-bullet 1 1 1) (make-alien 40 50)))
(define w2 (make-world (make-cannon 10) (make-bullet 1 1 -1) (make-alien 40 50)))

(define (world-template a-world)
  (... (world-cannon a-world)
   ... (world-bullet a-world)
   ... (world-alien a-world)))


; update-world : world -> world
(define (update-world a-world)
  (make-world
       (world-cannon a-world)
       (world-bullet a-world)
       (move-alien (world-alien a-world) 10 10)))

(check-expect (update-world (make-world (make-cannon 10)
                                        (make-bullet 1 1 1)
                                        (make-alien 40 90)))
              (make-world (make-cannon 10)
                          (make-bullet 1 1 1)
                          (make-alien 50 100)))
(check-expect (update-world (make-world (make-cannon 10)
                                        (make-bullet 1 1 -1)
                                        (make-alien 40 90)))
              (make-world (make-cannon 10)
                          (make-bullet 1 1 -1)
                          (make-alien 50 100)))
(check-expect (update-world (make-world (make-cannon 1)
                                        (make-bullet 1 1 1)
                                        (make-alien 50 100)))
              (make-world (make-cannon 1)
                          (make-bullet 1 1 1)
                          (make-alien 60 110)))

(define (world-handle-key a-world a-key)
  (make-world
   (cannon-handle-key (world-cannon a-world) a-key)
   (world-bullet a-world)
   (world-alien a-world)))

(check-expect (world-handle-key (make-world
                                 (make-cannon 1)
                                 (make-bullet 1 1 1)
                                 (make-alien 50 100)) "left")
              (make-world (make-cannon 0)
                          (make-bullet 1 1 1)
                          (make-alien 50 100)))
(check-expect (world-handle-key (make-world
                                 (make-cannon 1)
                                 (make-bullet 1 1 1)
                                 (make-alien 50 100)) "right")
              (make-world (make-cannon 2)
                          (make-bullet 1 1 1)
                          (make-alien 50 100)))
(check-expect (world-handle-key (make-world
                                 (make-cannon 1)
                                 (make-bullet 1 1 1)
                                 (make-alien 50 100)) " ")
              (make-world (make-cannon 1)
                          (make-bullet 1 1 1)
                          (make-alien 50 100)))

; Number 12
; draw-alien : alien, image -> image

(require "house-with-flowers.rkt")
(require 2htdp/image)
(define alien1 (make-alien 120 90))
(define alien2 (make-alien 1 90))

(define (draw-alien alien image)
  (place-image (circle 10 "solid" "red") (alien-x alien) (alien-y alien) image))

(check-expect (draw-alien alien1 house-with-flowers)
              (place-image (circle 10 "solid" "red") 120 90 house-with-flowers))
(check-expect (draw-alien alien2 house-with-flowers)
              (place-image (circle 10 "solid" "red") 1 90 house-with-flowers))

; draw-cannon : cannon, image -> image

(define cannon1 (make-cannon 90))
(define cannon2 (make-cannon 1))

(define (draw-cannon cannon image)
  (place-image (circle 10 "solid" "green") (cannon-number cannon) 1 image))

(check-expect (draw-cannon cannon1 house-with-flowers)
              (place-image (circle 10 "solid" "green") 90 1 house-with-flowers))
(check-expect (draw-cannon cannon2 house-with-flowers)
              (place-image (circle 10 "solid" "green") 1 1 house-with-flowers))

; draw-bullet : bullet, image -> image
(define bullet1 (make-bullet 1 1 1))
(define bullet2 (make-bullet 50 1 1))

(define (draw-bullet bullet image)
  (place-image (circle 4 "solid" "orange") (bullet-x bullet) (bullet-y bullet) image))

(check-expect (draw-bullet bullet1 house-with-flowers)
              (place-image (circle 4 "solid" "orange") 1 1 house-with-flowers))
(check-expect (draw-bullet bullet2 house-with-flowers)
              (place-image (circle 4 "solid" "orange") 50 1 house-with-flowers))

; draw-world : world -> image

(define (draw-world alien cannon bullet)
  ( overlay (draw-alien alien (empty-scene 160 90))
            (draw-cannon cannon (empty-scene 160 90))
            (draw-bullet bullet (empty-scene 160 90))))

(check-expect (draw-world alien1 cannon1 bullet1)
              (overlay (draw-alien alien1 (empty-scene 160 90))
                       (draw-cannon cannon1 (empty-scene 160 90))
                       (draw-bullet bullet1 (empty-scene 160 90))))

; Number 13
; Collision detection function


