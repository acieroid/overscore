(ns overscore.generator
  (:use clojure.java.io
        clojure.pprint
        overscore.musicxml))

(defmulti generate-note
  "Generate overtone code for a given note or chord"
  ;; Don't know why but class dispatch doesn't seem to work. So we use
  ;; a ugly hack to have somithing similar
  (fn [x] (str (class x))))

(defmethod generate-note "class overscore.musicxml.note" [n]
  `(~'play ~(:descr n) ~(:duration n)))

(defmethod generate-note "class overscore.musicxml.chord" [c]
  `(~'play-chord ~@(map generate-note (:notes c))))

(defn generate-bar
  "Generate overtone code for a given measure"
  [measure]
  `(~'bar
    (~'play-seq
     ~@(map generate-note (:notes measure)))))

(defn generate-prog
  "Generate overtone code for a given part"
  [part]
  `(~'defprog ~(symbol (:id part))
    ~@(map generate-bar (:bars part))))

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