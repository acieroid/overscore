;; Score available at
;; http://imslp.org/wiki/Cello_Suite_No.1_in_G_major,_BWV_1007_(Bach,_Johann_Sebastian)

;; Simple and beautiful classical 4/4 song
(ns overscore.examples.cello-suite
  (:use [overscore.notation]
        [overtone.inst.sampled-piano]))

;;; First staff
(defbar bar1
  (repeat-elements 2
     (simple-seq 1/4
      :G2 :D3 :B3 :A3
      :B3 :D3 :B3 :D3)))

(defbar bar2
  (repeat-elements 2
    (simple-seq 1/4
     :G2 :E3 :C4 :B3
     :C4 :E3 :C4 :E3)))

(defbar bar3
  (repeat-elements 2
    (simple-seq 1/4
     :G2 :F#3 :C4 :B3
     :C4 :F#3 :C4 :F#3)))

;;; Second staff
(defbar bar4
  (simple-seq 1/4
   :G2 :G3 :B3 :A3
   :B3 :G3 :B3 :G3
   :G2 :G3 :B3 :A3
   :B3 :G3 :B3 :F#3))

(defbar bar5
  (simple-seq 1/4
   :G2 :E3 :B3 :A3
   :B3 :G3 :F#3 :G3
   :E3 :G3 :F#3 :G3
   :B3 :D3 :C#3 :B3))

(defbar bar6
  (simple-seq 1/4
   :C#3 :G3 :A3 :G3
   :A3 :G3 :A3 :G3
   :C3 :G3 :A3 :G3
   :A3 :G3 :A3 :G3))

(defbar bar7
  (simple-seq 1/4
   :F#3 :A3 :D4 :C#4
   :D4 :A3 :G3 :A3
   :F#3 :A3 :G3 :A3
   :D3 :F#3 :E3 :D3))

;;; Third staff
(defbar bar8
  (repeat-elements 2
    (simple-seq 1/4
     :E2 :B2 :G3 :F#3
     :G3 :B2 :G3 :B2)))

(defbar bar9
  (simple-seq 1/4
   :E2 :C#3 :D3 :E3
   :D3 :C3 :B2 :A2
   :G3 :F#3 :E3 :D4
   :C#4 :B3 :A3 :G3))

(defbar bar10
  (simple-seq 1/4
   :F#3 :E3 :D3 :D4
   :A3 :D4 :F#3 :A3
   :D3 :E3 :F#3 :A3
   :G3 :F#3 :E3 :D3))

(defbar bar11
  (simple-seq 1/4
   :G#3 :D3 :F3 :E3
   :F#3 :D3 :G#3 :D3
   :B3 :D3 :F3 :E3
   :F#3 :D3 :G#3 :D3))

;;; Entire progression
(defprog cello
  bar1 bar2 bar3
  bar4 bar5 bar6 bar7
  bar8 bar9 bar10 bar11)

(defsong cello-suite
  [cello sampled-piano])
