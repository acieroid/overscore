(ns overscore.generator
  (:use overscore.utils
        clojure.java.io
        clojure.pprint
        overscore.musicxml))

(defmulti generate-note
  "Generate overtone code for a given note or chord"
  ;; Don't know why but class dispatch doesn't seem to work. So we use
  ;; a ugly hack to have somithing similar. To solve the problem, we
  ;; should have (= (class (->note :C4 1)) overscore.musicxml.note)
  ;; that returns true, but it isn't the case (but it was the case
  ;; before...)
  (fn [note] (str (class note))))

(defmethod generate-note "class overscore.musicxml.note" [n]
  `(~'play ~(:descr n) ~(:duration n)))

(defmethod generate-note "class overscore.musicxml.chord" [c]
  `(~'play-chord ~@(map generate-note (:notes c))))

(defmethod generate-note "class overscore.musicxml.note-seq" [s]
  `(~'play-seq ~@(map generate-note (:notes s))))

(defn generate-state-change
  "Generate a map that represent a change of state (time signature, tempo)"
  [state-change]
  (apply hash-map
         (concat
          (when (:time-signature state-change)
            [:time-signature (:time-signature state-change)])
          (when (:tempo state-change)
            [:tempo (:tempo state-change)]))))

(defn generate-bar
  "Generate overtone code for a given measure"
  [measure]
  (let [bar
        `(~'bar
          ~(if (= (count (:notes measure)) 1)
             (generate-note (first (:notes measure)))
             `(~'play-seq
               ~@(map generate-note (:notes measure)))))]
    (if (:state-change measure)
      `(~(generate-state-change (:state-change measure))
        ~bar)
      `(~bar))))

(defn generate-prog
  "Generate overtone code for a given part"
  [part]
  `(~'defprog ~(symbol (:id part))
     ~@(reduce
        (fn [bars cur]
          (concat bars (generate-bar cur)))
        nil (:bars part))))

(defn generate-song
  "Generate overtone code for an entire song (ie. a MusicXML file)"
  [song name]
  `(~@(map generate-prog (:progs song))
    (~'defsong ~name {:time-signature ~(:time-signature song)
                      :tempo ~(:tempo song)}
      ~@(map (fn [prog] [(symbol (:id prog)) 'sampled-piano])
             (:progs song)))))

(defn prelude
  "Returns the header of a generated file for the song with a given name"
  [name]
  `(~'ns ~name
     (:use [~'overscore.notation]
           [~'overtone.inst.sampled-piano])))

(defn write-to-file
  "Write a song (a MusicXML file) to an overtone file (a Clojure file)"
  [song name file]
  (with-open [out (writer file)]
    (pprint (prelude name) out)
    (.write out "\n")
    (doall
     (map #(do (pprint % out)
               ;; Let one line between each part
               (.write out "\n"))
          (generate-song song name)))))