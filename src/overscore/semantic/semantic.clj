;;; Find the semantic of the score given the position and class of segments
(ns overscore.semantic.semantic
  (:use overscore.utils
        overscore.recognition.segmentation.segment)
  (:require [clojure.xml :as xml]))

(defrecord classified-segment [start-x end-x start-y end-y class])
(defn to-classified-segment
  "Convert a five-element vector to a classified segment"
  [[start-x end-x start-y end-y class]]
  (->classified-segment start-x end-x start-y end-y class))

(defn group-vertically
  "Group segments that are vertically superposed. Return a list of
  list of segments, sorted from left to right."
  [segments]
  (let [left-to-right (sort #(compare (:start-x %1) (:start-x %2)) segments)]
    (loop [segs left-to-right
           res (transient [])
           cur-end -1
           cur-group []]
      (if (empty? segs)
        (rest (persistent! (conj! res cur-group)))
        (let [seg (first segs)]
          (if (<= (:start-x seg) cur-end)
            (recur (rest segs) res cur-end (cons seg cur-group))
            (recur (rest segs)
                   (conj! res
                          (sort #(compare (:start-y %1) (:start-y %2)) cur-group))
                   (:end-x seg)
                   [seg])))))))

(defn group-contains
  "Check if a group of segments contains a segment of a certain class"
  [class group]
  (not (empty?
        (filter #(= (:class %) class)
                group))))

(defn semantic
  "Find the semantic of a score given the notes positions and classes"
  [in-segments]
  (let [segments-vectors (read-vector in-segments)
        segments (map to-classified-segment segments-vectors)
        sorted (group-vertically segments)
        ;notes (interpret sorted)
        ;musicxml (to-musicxml notes)
        ]
    ;(xml/emit musicxml)
    ))