(ns asr.musicxml
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))

(defrecord musicxml-file [parts])
(defrecord part [id measures])
(defrecord measure [number notes])
(defrecord note [step octave duration])

(defn down-to
  "Similar to zip/down, but goes to a labelled node"
  [xml tag]
  (first (filter #(= (:tag %) tag) (:content xml))))


(defn is-note
  "Does the XML given as argument represents a note?"
  [xml]
  (= (:tag xml) :note))

(defn parse-note
  "Parse the XML of a note"
  [xml]
  (->note (-> xml
              (down-to :pitch)
              (down-to :step)
              :content
              first)
          (-> xml
              (down-to :pitch)
              (down-to :octave)
              :content
              first)
          (-> xml
              (down-to :duration)
              :content
              first)))

(defn is-measure
  "Does the XML given as argument represents a measure?"
  [xml]
  (= (:tag xml) :measure))

(defn parse-measure
  "Parse the XML of a measure"
  [xml]
  (->measure (Integer. (:number (:attrs xml)))
             (map parse-note
                  (filter is-note (:content xml)))))

(defn is-part
  "Does the XML given as argument represents a part?"
  [xml]
  (= (:tag xml) :part))

(defn parse-part
  "Parse the XML of a part"
  [xml]
  (->part (:id (:attrs xml))
          (map parse-measure
               (filter is-measure (:content xml)))))

(defn parse-musicxml
  "Parse the XML of a MusicXML file into a musicxml-file structure"
  [file]
  (let [xml (xml/parse file)]
    (->musicxml-file
     (map parse-part
          (filter is-part (:content xml))))))