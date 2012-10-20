(ns asc.notation
  (:use [overtone.music.rhythm :only [beat-ms]]
        [overtone.live :only [at]]
        [overtone.sc.node :only [kill]]
        [overtone.music.pitch :only [note]]))

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
    (let [duration-ms (state-beat-time state duration)]
      (if (not (= n :rest)) ; if it's a rest, we just doesn't play anything
        (at time
            (let [id (inst (note n))]
              (at (+ time duration-ms)
                  (kill id)))))
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
  ([& notes]
     (fn [state time inst]
       (reduce
        (fn [t n]
          (+ t (n state (+ time t) inst)))
        0 notes))))

(defn beat [n note]
  ^{:doc "Return a function that plays a note at beat n"}
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

;;; TODO: handle parameters
(defmacro bar [params & body]
  ^{:doc "Returns a bar, containing notes to be played"}
  `(fn [state# time# inst#]
     ;; doall is needed because of clojure's lazyness
     (doall
      (map (fn [n#] (n# state# time# inst#))
           (list ~@body)))
     (state-bar-time state#)))

(defmacro defbar [name params & body]
  ^{:doc "Defines a bar"}
  `(def ~name (bar ~params ~@body)))

;; TODO: find a more appropriate name (does not only apply to a bar,
;; but also to a sequence of notes, ...)
(defn repeat-bars [n & bars]
  ^{:doc "Repeat a set of bars multiple time"}
  (apply play-seq (flatten (repeat n bars))))

(defmacro prog [& body]
  ^{:doc "Returns a bar, containing bars to be played"}
  `(fn [state# time# inst#]
       (reduce
        (fn [t# n#]
          (+ t# (n# state# (+ time# t#) inst#)))
        0 (list ~@body))))

(defmacro defprog [name & body]
  ^{:doc "Defines a progression, containing bars to be played"}
  `(def ~name (prog ~@body)))

(defmacro song [& progs]
  ^{:doc "Returns a song, containing progressions to be played with specific instruments"}
  `(fn [state#]
     (let [time# (now)]
       (map (fn [descr#]
              ;;; descr is composed of [progression instrument]
              ((first descr#) state# time# (second descr#)))
            (list ~@progs)))))

(defmacro defsong [name & progs]
  ^{:doc "Defines a song, containing progressiosn to be played with specific instruments"}
  `(def ~name (song ~@progs)))
