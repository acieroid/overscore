;;; This module parses a MusicXML file and return a structure
;;; representing it. The MusicXML semantics are described there:
;;; http://www.makemusic.com/musicxml/specification/dtd/

(ns asr.musicxml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))

(defrecord song [progs])
(defrecord prog [id bars])
(defrecord bar [number notes])
(defrecord note [descr duration])

(defn down-to
  "Similar to zip/down, but goes to a labelled node"
  [xml tag]
  (first (filter #(= (:tag %) tag) (:content xml))))

(defn is-note
  "Does the XML given as argument represents a note?"
  [xml]
  (= (:tag xml) :note))

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
                                      :content first Integer.)]
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
                        :content first Integer.)))
      ;; No pitch, so it is a rets
      :rest)))

(defn parse-note
  "Parse the XML of a note"
  [xml divisions]
  (->note (note-descr xml)
          (-> xml
              (down-to :duration)
              :content first Double.
              ;; Divide the MusicXML duration by the number of
              ;; divisions to obtain the real duration
              ;; TODO: converting to a fraction if possible would be neat
              (/ divisions))))

(defn is-measure
  "Does the XML given as argument represents a measure?"
  [xml]
  (= (:tag xml) :measure))

(defn parse-measure
  "Parse the XML of a measure"
  [xml & [divisions]]
  (let [divs-str (-> xml
                     (down-to :attributes)
                     (down-to :divisions)
                     :content first)
        divs (if divs-str
               (Integer. divs-str)
               (if divisions
                 divisions
                 (throw (Throwable. "No division attribute previously defined"))))]
    (list divs
     (->bar (Integer. (:number (:attrs xml)))
            (map #(parse-note % divs)
                 (filter is-note (:content xml)))))))

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

(defn parse-musicxml
  "Parse the XML of a MusicXML file into a musicxml-file structure"
  [file]
  (let [xml (xml/parse file)]
    (->song
     (map parse-part
          (filter is-part (:content xml))))))