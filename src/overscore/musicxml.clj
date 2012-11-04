;;; This module parses a MusicXML file and return a structure
;;; representing it. The MusicXML semantics are described there:
;;; http://www.makemusic.com/musicxml/specification/dtd/

(ns overscore.musicxml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))

(defrecord song [time-signature tempo progs])
(defrecord prog [id bars])
(defrecord bar [number notes])
(defrecord chord [notes])
(defrecord note-seq [notes])
(defrecord note [descr duration])

;; TODO: default value when n is nil ?
(defn parse-int
  "Parse an integer, possibly from a floating-point representation"
  [n]
  (try
    (int (Double. n))
    (catch Exception e
      (println "Error when parsing integer from" n ":" e))))

(defn first-elem-as-int
  "Return the firts element of the :contents attribute as an integer"
  [x] (-> x
          :content first parse-int))

(defn down-to
  "Similar to zip/down, but goes to a labelled node"
  [xml tag]
  (first (filter #(= (:tag %) tag) (:content xml))))

(defn is-note
  "Does the XML given as argument represents a note?"
  [xml]
  (and (= (:tag xml) :note)
       ;; Grace notes are just ornaments, not actual notes
       (= nil (down-to xml :grace))))

(defn note-descr
  "Return the description of a note. Eg. for a G on the second octave, :G2. For a rest, :rest"
  [xml]
  (let [pitch (down-to xml :pitch)]
    (if pitch
      ;; Has a pitch, so it is a note
      (keyword (str (-> pitch
                        (down-to :step)
                        :content first)
                    (if (down-to pitch :alter)
                      (let [alter (-> pitch
                                      (down-to :alter)
                                      first-elem-as-int)]
                        (case alter
                          1 "#"
                          -1 "b"
                          (throw (Throwable.
                                  (str "Alteration of more than one tone are not yet implemented"
                                       " (alter was set to " alter
                                       ")")))))
                      "")
                    (-> xml
                        (down-to :pitch)
                        (down-to :octave)
                        first-elem-as-int)))
      ;; No pitch, so it is a rets
      :rest)))

(defn parse-note
  "Parse the XML of a note"
  [xml divisions]
  (->note (note-descr xml)
          (-> xml
              (down-to :duration)
              first-elem-as-int
              ;; Divide the MusicXML duration by the number of
              ;; divisions to obtain the real duration
              (/ divisions))))

(defn is-measure
  "Does the XML given as argument represents a measure?"
  [xml]
  (= (:tag xml) :measure))

(defmulti add-to-chord
  "Add a note to a chord, or build a chord from two notes"
  ;; Same ugly hack as for generator's generate-note
  (fn [chord _] (str (class chord))))

(defmethod add-to-chord "" [_ note]
  (->chord [note]))

(defmethod add-to-chord "class overscore.musicxml.note" [n1 n2]
  (->chord [n2 n1]))

(defmethod add-to-chord "class overscore.musicxml.chord" [chord note]
  (->chord (cons note (:notes chord))))

(defmulti reverse-chord
  "Reverse the order of notes in a chord (used to have the notes in the same order as in the MusicXML file)"
  (fn [chord] (str (class chord))))

(defmethod reverse-chord "" [c] c)
(defmethod reverse-chord "class overscore.musicxml.note" [note] note)
(defmethod reverse-chord "class overscore.musicxml.chord" [chord]
  (->chord (reverse (:notes chord))))

(defn is-chord
  "Is a XML note part of a chord?"
  [xml]
  (down-to xml :chord))

(defn parse-measure-helper [notes divs]
  (let [state
        (reduce
         (fn [st el]
           (let [last-note (first st)
                 notes (second st)
                 note (parse-note el divs)]
             (if (is-chord el)
               ;; Add this note to the current chord
               [(add-to-chord last-note note)
                notes]
               ;; Last note wasn't in the same chord, push it
               [note
                (if last-note
                  (cons (reverse-chord last-note) notes)
                  notes)])))
         [nil nil] ;; Initial state
         notes)]
    (reverse
     (cons (reverse-chord (first state))
           (second state)))))

(defn group-by-voice
  "Group the notes by voice"
  [notes]
  (map second
       (group-by (fn [xml]
                   (-> xml
                       (down-to :voice)
                       :content first))
                 notes)))

(defn to-seq-if-multiple
  "Convert a group of note to a note-seq if there are more than one note, else return the only note"
  [notes]
  (if (= (count notes) 1)
    (first notes)
    (->note-seq notes)))

(defn parse-measure
  "Parse the XML of a measure"
  [xml & [divisions]]
  (let [divs-str (-> xml
                     (down-to :attributes)
                     (down-to :divisions)
                     :content first)
        divs (if divs-str
               (parse-int divs-str)
               (if divisions
                 divisions
                 (throw (Throwable. "No division attribute previously defined"))))]
    (list divs
          (let [voices
                (map #(parse-measure-helper % divs)
                     (group-by-voice (filter is-note (:content xml))))]
            (->bar (parse-int (:number (:attrs xml)))
                   (if (== (count voices) 1)
                     ;; Only one voice, no "main" chord
                     (first voices)
                     ;; More than one voice, use a chord
                     [(->chord (map to-seq-if-multiple voices))]))))))

(defn is-part
  "Does the XML given as argument represents a part?"
  [xml]
  (= (:tag xml) :part))

(defn parse-part
  "Parse the XML of a part"
  [xml]
  (->prog (:id (:attrs xml))
          (reverse
           (second
            (reduce (fn [st el]
                      (let [res (parse-measure el (first st))]
                        (list (first res)
                              (cons (second res) (second st)))))
                    nil
                    (filter is-measure (:content xml)))))))

(defn parse-time-signature
  "Return the first time signature of the song"
  [xml]
  ;; The first time signature has to be in the first measure
  ;; (otherwise what would be its time signature?)
  (let [time (-> xml
                 (down-to :part)
                 (down-to :measure)
                 (down-to :attributes)
                 (down-to :time)
                 :content)]
    (if time
      (seq
       (-> time
           (down-to :beats)
           first-elem-as-int)
       (-> time
           (down-to :beat-type)
           first-elem-as-int))
      ;; defaults to 4-4 (common time)
      [4 4])))

(defn parse-tempo
  "Return the first tempo of the song"
  [xml]
  ;; Same remark as for parse-time-signature
  (let [tempo (-> xml
                  (down-to :part)
                  (down-to :measure)
                  (down-to :direction)
                  (down-to :sound)
                  :attributes :tempo)]
    (if tempo
      (parse-int tempo)
      ;; defaults to 80 bpm
      80)))

(defn parse-musicxml
  "Parse the XML of a MusicXML file into a musicxml-file structure"
  [file]
  (let [xml (xml/parse file)]
    (->song
     (parse-time-signature (:content xml))
     (parse-tempo (:content xml))
     (map parse-part
          (filter is-part (:content xml))))))