(ns asr.generator
  (:use clojure.java.io
        clojure.pprint
        asr.musicxml))

(defn generate-note
  "Generate overtone code for a given note"
  [note]
  (let [t (type note)]
    (cond
     (= t asr.musicxml.note)
     `(~'play ~(:descr note) ~(:duration note))
     (= t asr.musicxml.chord)
     `(~'play-chord ~@(map generate-note (:notes note))))))

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
    (~'defsong ~name
      ;;; TODO: find a way to specify instruments ?
      ~@(map (fn [prog] [(symbol (:id prog)) 'sampled-piano])
             (:progs song)))))

(defn prelude
  "Returns the header of a generated file for the song with a given name"
  [name]
  `(~'ns ~name
     (:use [~'asr.notation]
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