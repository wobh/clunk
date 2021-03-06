;;;; -*- Mode:lisp;coding:utf-8 -*-
;;;; FILE: revolver-roulette.lisp

;;;; DESCRIPTION

;;;; "Russian Roulette" in Common Lisp

;;;; AUTHORS

;;;; William H. Clifford <wobh@yahoo.com>

;;;; NOTES

;;;; http://www.straightdope.com/columns/read/1353/did-the-russians-ever-play-russian-roulette

;;;; home.online.no/~jpthing/games/roulette.html

(defpackage "ORG.WOBH.REVOLVER-ROULETTE"
  (:nicknames "RUSSIAN-ROULETTE")
  (:use "COMMON-LISP")
  (:export "MAIN")
  (:documentation "Don't try this at home."))

(defparameter *revolver-random-state* (make-random-state T))

(defun shuffle-seq (seq &optional (random-state *revolver-random-state*))
  "Knuth shuffle a sequence"
  (let ((len (length seq)))
    (dotimes (i len seq)
      (rotatef (elt seq i)
	       (elt seq (+ i (random (- len i) random-state)))))))

(defun rotate-seq (seq &optional (count 1))
  "Rotate a seq by count left (or right if given a negative count)."
  (let* ((seq-length (length seq))
	 (count (mod (- count) seq-length))
	 (dif-seq-length-count (- seq-length count))
    	 (temp (subseq seq dif-seq-length-count))
	 (result (make-array (length seq))))
    (replace result seq :start1 (- seq-length dif-seq-length-count))
    (replace result temp)))


;;;; Revolver

(defstruct (revolver
	     (:conc-name gun-)
	     (:constructor make-revolver (&key (chambers 6))))
  (cylinder (make-array chambers :element-type 'bit :initial-element 0)))

(defun gun-chambers (gun)
  "How many chambers does this gun have?"
  (length (gun-cylinder gun)))

(defun gun-bullets (gun)
  "How many bullets are loaded in this gun?"
  (loop
     for chambers across (gun-cylinder gun)
     sum chambers))

(defun list-unloaded-chambers (gun)
  (loop
     for chamber from 0 upto (gun-chambers gun)
     collect (when (zerop (elt (gun-cylinder gun) chamber))
	       chamber)))

(defun spin-cylinder (gun)
  (setf (gun-cylinder gun)
	(rotate-seq (gun-cylinder gun)
		    (random (gun-chambers gun) *revolver-random-state*))))

(defun random-load-cylinder (bullets gun)
  (assert (< 0 bullets))
  (assert (< (+ bullets (gun-bullets gun)) (gun-chambers gun)))
  (let ((free-chambers (shuffle-seq (list-unloaded-chambers gun))))
    (loop
       repeat bullets
       do
	 (setf (elt (gun-cylinder gun) (pop free-chambers)) 1))))

(defun load-gun (bullets gun)
  (assert (< 0 bullets))
  (assert (< (+ bullets (gun-bullets gun)) (gun-chambers gun)))
  (nsubstitute 1 0 (gun-cylinder gun) :count bullets))

(defun unload-gun (bullets gun)
  (assert (< 0 bullets))
  (assert (< (+ bullets (gun-bullets gun)) (gun-chambers gun)))
  (nsubstitute 0 1 (gun-cylinder gun) :count bullets))

(defun fire-gun (gun)
  (assert (typep (gun-chambers gun) (list 'integer 1 (gun-bullets gun))))
  (unless (zerop (elt (gun-cylinder gun) 0))
    (setf (elt (gun-cylinder gun) 0) 0))
  (setf (gun-cylinder gun) (rotate-seq (gun-cylinder gun) 1)))


;;;; Game

(defparameter *game-events*
  '((setup-game        (select-gun))
    (select-gun        (load-gun))
    (load-gun          (invite-players))
    (invite-players    (player-joins-game
			player-declines))
    (player-declines   (invite-players))
    (player-joins-game (start-game))
    (start-game        (first-player))
    (next-player       (spin-chamber
			pull-trigger))
    (spin-chamber      (spin-chamber
			pull-trigger))
    (pull-trigger      (gun-fires
			gun-does-not-fire))
    (gun-does-not-fire (next-player))
    (gun-fires         (player-dies))
    (player-dies       (game-ends
			next-player))
    (game-ends         Nil)))

(defparameter *players*)

(defstruct (game)
  (gun     (make-revolver))
  (players ())
  (seats   ())
  (history ()))

(defun list-empty-seats )

(defun game-add-player (player seat game)
  (push player (game-players game))
  ())

(defun get-player-at-seat (seat game)
  ())

(defun record-event (event game)
  (push event (game-events)))

(defun latest-event (game)
  (first (game-events hunt)))

(defun events-since (last-event game)
  (loop
     for event in (game-events game)
     collect event
     until (eq event last-event)))

;;;; Play game

(defun game-turn (player game)
  (when (spin-cylinder-p)
    (spin-cylinder (game-gun game)))
  (cond ((fire-gun)
	 (record-event (make-event player 'fires-bullet))
	 (record-event (make-event player 'dies)))
	(T
	 (record-event (make-event player 'hammer-click))))
  (events-since (make-event player 'pulls-trigger)))

(defun game-round (game)
  (loop
     for player in (game-seats game)
     do
       ()))

(defun play (game)
  (for)))

;; start-game
;; watch-game
;; pick-gun
;; invite-player
;; stand-up

;; join-game
;; pick-seat
;; quit-game

;; register-bet


(defparameter *currency* '(koipak roibal))

