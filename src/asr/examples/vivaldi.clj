;;; Score available at
;;; http://imslp.org/wiki/Violin_Concerto_in_E_major,_RV_269_(Vivaldi,_Antonio)#Recordings

(ns asr.examples.vivaldi
  (:use [asr.notation]
        [overtone.inst.synth]))

;;; Main violin
(defbar bar0-MV
  (simple-seq
   3.5 :rest
   1/2 :E4))

(defbar bar1-MV
  (simple-seq
   1/2 :G#4 :G#4 :G#4
   1/4 :F#4 :E4
   3/2 :B4
   1/4 :B4 :A4))

(defbar bar3-MV
  (play-seq
   (play :G#4 1/2)
   (play :A4 1/4)
   (play :B4 1/4)
   (play :A4 1/2)
   (play :G#4 1/2)
   (play :F#4 1/2)
   (play :D#4 1/2)
   (play :B3 1/2)
   (play :E4 1/2)))

(defbar bar6-MV
  (play-seq
   (play :G#4 1/2)
   (play :A4 1/4)
   (play :B4 1/4)
   (play :A4 1/2)
   (play :G#4 1/2)
   (play :F#4 1)
   (play :rest 1/4)
   (play :E4 1/4)))

(defprog main-violin
  bar0-MV bar1-MV bar1-MV bar3-MV
  bar1-MV bar1-MV bar6-MV)

;;; First violin
(defprog first-violin
  bar0-MV bar1-MV bar1-MV bar3-MV
  bar1-MV bar1-MV bar6-MV)

;;; Second violin
(defbar bar0-SV
  (play-seq
   (play :rest 3.5)
   (play :B3 1/2)))

(defbar bar1-SV
  (play-seq
   (play :E4 1/2)
   (play :E4 1/2)
   (play :E4 1/2)
   (play :E4 1/2)
   (play :G#4 3/2)
   (play :G#4 1/4)
   (play :F#4 1/4)))

(defbar bar3-SV
  (play-seq
   (play :E4 1/2)
   (play :F#4 1/4)
   (play :G#4 1/4)
   (play :F#4 1/2)
   (play :E4 1/2)
   (play :D#4 1)
   (play :rest 1/2)
   (play :B3 1/2)))

(defbar bar6-SV
  (play-seq
   (play :E4 1/2)
   (play :F#4 1/4)
   (play :G#4 1/4)
   (play :F#4 1/2)
   (play :E4 1/2)
   (play :D#4 1)
   (play :rest 1/2)
   (play :G#4 1/2)))

(defprog second-violin
  bar0-SV bar1-SV bar1-SV bar3-SV
  bar1-SV bar1-SV bar6-SV)

;;; Full song
(defsong vivaldi
  [main-violin pad]
  [first-violin pad]
  [second-violin pad])
