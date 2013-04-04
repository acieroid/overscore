;;; Find the semantic of the score given the position and class of segments
(ns overscore.semantic.semantic
  (:use clojure.java.io
        overscore.utils
        overscore.recognition.segmentation.segment)
  (:require [clojure.xml :as xml]))

(defrecord score-system [clef time notes])
(defrecord score-rest [type])
(defrecord score-note [step octave duration])

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

(defn group-extract
  "Extract one element of a certain class from a group"
  [group classes]
  (let [is-among (fn [class classes]
                   (not (empty? (filter #(= class %) classes))))]
    (first (filter #(is-among (:class %) classes) group))))

(defn group-contains
  "Check if a group of segments contains a segment of a certain
  class (among the given classes)"
  [group classes]
  (not (nil? (group-extract group classes))))

(defn interpret-note
  "Convert a group of symbols to a note"
  [pre beam head post refs staves]
  ;; TODO
  (->score-note "A" 4 1))

(defn parse
  "Parse a non-terminal from the groups of note"
  [groups symbol refs staves]
  (let [rests [:quarter_rest :height_rest :one_16th_rest]
        accidentals [:sharp :flat :natural]
        dots [:dot_set]
        times [:common_time :cut_time :time_four :time_four_four :time_six_eight
               :time_three :time_three_four :time_two :time_two_four]
        clefs [:g_clef :g_clef_8vb :f_clef :c_clef]
        noteheads [:notehead_black :notehead_black_2 :notehead_black_3
                   :notehead_void :notehead_void_2
                   :whole_note :whole_note_2]
        beams [:beam :beam_hook]
        flags [:flag_1 :flag_1_up :flag_2 :flag_2_up]
        time-default :time_four_four
        clef-default :g_clef
        notes-first (concat accidentals rests beams noteheads)]
    (println symbol (first groups))
    (case symbol
      :system (let [[clef groups'] (parse groups :clef refs staves)
                    [time groups''] (parse groups' :time refs staves)
                    [notes rest] (parse groups'' :notes refs staves)]
                [(->score-system clef time notes) rest])
      :notes (let [[note groups'] (parse groups :note refs staves)
                   [notes groups'']
                   (if (group-contains (first groups) notes-first)
                     (parse groups' :notes refs staves)
                     [[] groups'])]
               [(cons note notes) groups''])
      :note (if (group-contains (first groups) rests)
              (let [seg (group-extract (first groups) rests)]
                [(->score-rest (:class seg)) (rest groups)])
              (let [[pre groups'] (parse groups :pre refs staves)
                    [[beam head] groups''] (parse groups' :note_body refs staves)
                    [post groups'''] (parse groups'' :post refs staves)]
                ; TODO: interpret pitch from vertical position
                [(interpret-note pre beam head post refs staves) groups''']))
      :pre (if (group-contains (first groups) accidentals)
             (let [seg (group-extract (first groups) accidentals)]
               [(:class seg) (rest groups)])
             [:none groups])
      :post (if (group-contains (first groups) dots)
              (let [seg (group-extract (first groups) dots)]
                [(:class seg) (rest groups)])
              (parse groups :flag refs staves))
      :note_body (let [[beam groups'] (parse groups :beam refs staves)
                       [notehead groups''] (parse groups' :notehead refs staves)]
                   [[beam notehead] groups''])
      :time (if (group-contains (first groups) times)
              (let [seg (group-extract (first groups) times)]
                [(:class seg) (rest groups)])
              [time-default groups])
      :clef (if (group-contains (first groups) clefs)
              (let [seg (group-extract (first groups) clefs)]
                [(:class seg) (rest groups)])
              [clef-default groups])
      :notehead (if (group-contains (first groups) noteheads)
                  (let [seg (group-extract (first groups) noteheads)]
                    [seg (rest groups)])
                  [:none groups])
      :beam (if (group-contains (first groups) beams)
              (let [seg (group-extract (first groups) beams)]
                [(:class seg) (rest groups)])
              [:none groups])
      :flag (if (group-contains (first groups) flags)
              (let [seg (group-extract (first groups) flags)]
                [(:class seg) (rest groups)])
              [:none groups]))))

(defn interpret
  "Interpret the groups of symbols as notes that forms a score"
  [groups refs staves]
  (let [[system rest] (parse groups :system refs staves)]
    (if (empty? rest)
      system
      ;; TODO
      (do (println (first rest))
          system))))

(defn time-to-musicxml
  [time]
  (let [[beats beat-type]
        (if (nil? time)
          [4 4] ;; 4-4 by default
          (case time
            :common_time [4 4]
            :cut_time [2 2]
            :time_four [4 4]
            :time_four_four [4 4]
            :time_six_eight [6 8]
            :time_three [3 4]
            :time_three_four [3 4]
            :time_two [2 4]
            :time_two_four [2 4]))]
    {:tag :time :attrs nil
     :content [{:tag :beats :attrs nil :content [(str beats)]}
               {:tag :beat-type :attrs nil :content [(str beat-type)]}]}))

(defn clef-to-musicxml
  [clef]
  (let [[sign line]
        (if (nil? clef)
          ["G" 2] ;; G clef by default
          (case clef
            :g_clef ["G" 2]
            :g_clef_8vb ["G" 2]
            :f_clef ["F" 4]
            :c_clef ["C" 4]))]
    {:tag :clef :attrs {:number "1"}
     :content [{:tag :sign :attrs nil :content [sign]}
               {:tag :line :attrs nil :content [(str line)]}]}))

(defn note-to-musicxml
  [note]
  {:tag :note :attrs nil
   :content [{:tag :pitch :attrs nil
              :content [{:tag :step :attrs nil :content [(:step note)]}
                        {:tag :octave :attrs nil :content [(str (:octave note))]}]}
             {:tag :duration :attrs nil :content [(str (:duration note))]}]})

(defn system-to-musicxml
  "Convert a system to MusicXML data"
  [system]
  ;; Only one measure for the moment
  [{:tag :measure :attrs {:number "1"}
    :content (cons {:tag :attributes :attrs nil
                    :content [{:tag :divisions :attrs nil
                               :content ["1"]}
                              {:tag :key :attrs nil :content []}
                              {:tag :staves :attrs nil :content ["1"]}
                              (clef-to-musicxml (:clef system))]}
                   (map note-to-musicxml (:notes system)))}])

(defn to-musicxml
  "Convert what 'interpret' computed into MusicXML"
  [score]
  {:tag :score-partwise :attrs {:version "3.0"}
   :content [{:tag :part-list :attrs nil
              :contents [{:tag :score-part :attrs {:id "P1"}
                          :content []}]}
             {:tag :part :attrs {:id "P1"}
              :content (system-to-musicxml score)}]})

(defn semantic
  "Find the semantic of a score given the notes positions and classes"
  [in-classes in-refs in-staves out-xml]
  (let [segments-vectors (read-vector in-classes)
        refs (read-vector in-refs)
        staves (read-vector in-staves)
        segments (map to-classified-segment segments-vectors)
        sorted (group-vertically segments)
        score (interpret sorted refs staves)
        musicxml (to-musicxml score)]
    (binding [*out* (writer out-xml)]
      (xml/emit musicxml))))
