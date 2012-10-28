(ns asr.generator
  (:use [clojure.java.io]))

(defn note->keyword
  "Convert a note record to a keyword representing it"
  [note]
  (keyword (str (:step note) (:octave note))))

;;; TODO: use the time signature
(defn generate-note
  "Generate overtone code for a given note"
  [note]
  `(~'play ~(note->keyword note) ~(/ (:duration note) 4)))

(defn generate-measure
  "Generate overtone code for a given measure"
  [measure]
  `(~'bar
    (~'play-seq
     ~@(map generate-note (:notes measure)))))

(defn generate-part
  "Generate overtone code for a given part"
  [part]
  `(~'defprog ~(symbol (:id part))
    ~@(map generate-measure (:measures part))))

(defn generate-song
  "Generate overtone code for an entire song (ie. a MusicXML file)"
  [song name]
  `(~@(map generate-part (:parts song))
    (~'defsong ~name
      ;;; TODO: find a way to specify instruments ?
      ~@(map (fn [part] [(symbol (:id part)) 'sampled-piano])
             (:parts song)))))

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