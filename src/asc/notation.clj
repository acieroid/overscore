(ns asc.notation
  (:require [overtone.core]))

(defrecord state [bpm time-signature])

;;; TODO: handle time-signature
(defn state-bar-time [state]
  ^{:doc "Return the duration of a bar in ms"}
  (beat-ms 4 (:bpm state)))

;;; TODO: handle time-signature
(defn state-beat-time [state n]
  ^{:doc "Return the duration of n beats in ms"}
  (beat-ms n (:bpm state)))

;;; TODO: handle time-signature
(defn play
  ^{:doc "Returns a note to be played during a certain duration"}
  [n duration]
  (fn [state time inst]
    ;; The instrument should take a midi note as input. If it needs a
    ;; frequency, it should use midi->hz to convert it
    (let [duration-ms (* duration (beat-ms 1 (:bpm state)))]
      (at time
          (let [id (inst (note n))]
            (at (+ time duration-ms)
                (kill id))))
      duration-ms)))

(defn play-chord
  ^{:doc "Returns a function that plays multiple notes at the same time"}
  [& notes]
  (fn [state time inst]
    (apply max
           (map (fn [n] (n state time inst))
                notes))))

(defn play-seq
  ^{:doc "Return a function that plays multiple notes in sequence"}
  ([] 0)
  ([& notes]
     (fn [state time inst]
       (reduce
        (fn [t n]
          (+ t (n state (+ time t) inst)))
        0 notes))))

(defn beat [n note]
  ^{:doc "Return a function that plays a note at beat n"}
  (fn [state time inst]
    (note state (+ time (state-beat-time state n)) inst)))

;;; TODO: handle parameters
(defmacro defbar [name params & body]
  ^{:doc "Defines a bar, containing notes to be played"}
  `(def ~name
     (fn [state# time# inst#]
       ;; doall is needed because of clojure's lazyness
       (doall
        (map (fn [n#] (n# state# time# inst#))
             (list ~@body)))
       (state-bar-time state#))))

;;; TODO: define beat, defprog, defsong

;;; For development/debugging only
(use 'overtone.inst.synth)
(def default-state (->state 80 [4 4]))
(def default-inst pad)
(defn test-play [n]
  (n default-state (now) default-inst))

(defbar foo {}
  (play-chord
   (play :C4 1/2)
   (play :E4 1/2)
   (play :G4 1/2)))

(defbar foo {:time-signature [4 4]
             :bpm 80}
  (play-seq
   (play-chord
    (play :C4 1/2)
    (play :E4 1/2)
    (play :G4 1/2))
   (play :D4 1/2)
   (play :E4 1/2)
   (play :F4 1/2)
   (play :G4 1/2)
   (play :A4 1/2)
   (play :B4 1/2)))

(defbar foo {}
  (play :C4 1)
  (beat 1 (play :E4 1))
  (beat 2 (play :G4 1))
  (beat 3 (play :E4 1)))