(ns asr.notation
  (:use [overtone.music.rhythm :only [beat-ms]]
        [overtone.music.time :only [now]]
        [overtone.live :only [at]]
        [overtone.sc.node :only [kill]]
        [overtone.music.pitch :only [note]]
        [overtone.sc.server :only [stop]]))

(defrecord state [bpm time-signature])

;;; TODO: handle time-signature
(defn state-bar-time
  "Return the duration of a bar in ms"
  [state]
  (beat-ms 4 (:bpm state)))

;;; TODO: handle time-signature
(defn state-beat-time
  "Return the duration of n beats in ms"
  [state n]
  (beat-ms n (:bpm state)))

;;; TODO: handle time-signature
(defn play
  "Returns a note to be played during a certain duration"
  [n duration]
  (fn [state time inst]
    ;; The instrument should take a midi note as input. If it needs a
    ;; frequency, it should use midi->hz to convert it
    (let [duration-ms (state-beat-time state duration)]
      (if (not (= n :rest)) ; if it's a rest, we just doesn't play anything
        (at time
            (let [id (inst (note n))]
              (at (+ time duration-ms)
                  (kill id)))))
      duration-ms)))

(defn play-chord
  "Returns a function that plays multiple notes at the same time"
  [& notes]
  (fn [state time inst]
    (apply max
           (map (fn [n] (n state time inst))
                notes))))

(defn play-seq
  "Return a function that plays multiple notes in sequence"
  [& notes]
  (fn [state time inst]
    (reduce
     (fn [t n]
       (+ t (n state (+ time t) inst)))
     0 notes)))

(defmacro simple-seq
  "Simplify the write of multiple notes in sequence. Takes multiple
notes and duration as arguments. Each time a note is encountered, it
is added to be played with the current duration. Each time a duration
is encountered, the current duration is changed to match it. The
default duration is 1"
  [& body]
  (let [notes (reverse
               (first
                (reduce
                 (fn [state elem]
                   (let [notes (first state)
                         duration (second state)]
                     (cond
                      ;; A note, keep the same duration and play it
                      (keyword? elem) [(cons `(play ~elem ~duration) notes)
                                       duration]
                      ;; A change of duration
                      (number? elem) [notes elem])))
                 ;; Default duration is 1
                 [[] 1]
                 body)))]
    `(play-seq ~@notes)))

(defn beat
  "Return a function that plays a note at beat n"
  [n note]
  (fn [state time inst]
    (let [delta (state-beat-time state n)]
      (+ delta
         (note state (+ time (state-beat-time state n)) inst)))))

;;; Note that bar, prog and song could be defined as functions, but
;;; defining them as macros permits to redefine the elements they
;;; depends on, without having to re-eval their definition. For
;;; example, if progression p contains bar b, when the definition of b
;;; is modified, p will automatically use the new definition. Having
;;; those macros as functions, it would be necessary to re-evaluate
;;; p's definition to have it to use the new b.

(defmacro bar
  "Returns a bar, containing notes to be played"
  [& body]
  `(fn [state# time# inst#]
     ;; doall is needed because of clojure's lazyness
     (doall
      (map (fn [n#] (n# state# time# inst#))
           (list ~@body)))
     (state-bar-time state#)))

(defmacro defbar
  "Defines a bar"
  [name & body]
  `(def ~name (bar ~@body)))

(defn repeat-elements
  "Repeat a set of bars multiple time"
  [n & bars]
  (apply play-seq (flatten (clojure.core/repeat n bars))))

(defmacro prog
  "Returns a bar, containing bars to be played"
  [& body]
  `(fn [state# time# inst#]
       (reduce
        (fn [t# n#]
          (+ t# (n# state# (+ time# t#) inst#)))
        0 (list ~@body))))

(defmacro defprog
  "Defines a progression, containing bars to be played"
  [name & body]
  `(def ~name (prog ~@body)))

(defmacro song
  "Returns a song, containing progressions to be played with specific instruments"
  [& progs]
  `(fn [state#]
     (let [time# (now)]
       (map (fn [descr#]
              ;;; descr is composed of [progression instrument]
              ((first descr#) state# time# (second descr#)))
            (list ~@progs)))))

(defmacro defsong
  "Defines a song, containing progressiosn to be played with specific instruments"
  [name & progs]
  `(def ~name (song ~@progs)))

(defn start
  "Start a song."
  ([song] (song (->state 80 [4 4])))
  ([song bpm] (song (->state bpm [4 4]))))

(defn start-element
  "Start an element of a song (progression, bar or note) with a given instrument"
  ([elem inst] (elem (->state 80 [4 4]) (now) inst))
  ([elem inst bpm] (elem (->state bpm [4 4]) (now) inst)))