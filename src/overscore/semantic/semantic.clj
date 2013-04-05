;;; Find the semantic of the score given the position and class of segments
(ns overscore.semantic.semantic
  (:use clojure.java.io
        clojure.contrib.prxml
        overscore.utils
        overscore.recognition.segmentation.segment))

(defrecord score-system [clef time notes])
(defrecord score-note [step octave duration])
;; Default division
(def divisions 64)

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

(defn refine-groups
  "Refine the groups by grouping pre and post symbols with the
  neighbour group containing a notehead, if they are not already in a
  group with a note head"
  [groups]
  (let [pre [:sharp :flat :natural :beam :beam_hook]
        post [:flag_1 :flag_1_up :flag_2 :flag_2_up :dot_set]
        notehead [:notehead_black :notehead_black_2 :notehead_black_3
                  :notehead_void :notehead_void_2
                  :whole_note :whole_note_2]]
    (loop [groups groups
           res (transient [])
           cur []]
      (if (empty? groups)
        (persistent! (conj! res cur))
        (if (group-contains (first groups) notehead)
          ;; Notehead
          (if (group-contains cur notehead)
            ;; Already contains a notehead, end of this group
            (recur (rest groups)
                   (conj! res cur)
                   (first groups))
            ;; Add the notehead to this group
            (recur (rest groups)
                   res
                   (concat (first groups) cur)))
          (if (group-contains (first groups) post)
            ;; Post, add to the current group
            (recur (rest groups)
                   res
                   (concat (first groups) cur))
            (if (group-contains (first groups) pre)
              ;; Pre
              (if (group-contains cur notehead)
                ;; Pre after a notehead, create a new group
                (recur (rest groups)
                       (conj! res cur)
                       (first groups))
                ;; Pre before a notehead, add it to the current  group
                (recur (rest groups)
                       res
                       (concat (first groups) cur)))
              ;; No pre, post or notehead, finish the current group and
              ;; add this one too
              (recur (rest groups)
                     (if (empty? cur)
                       (conj! res (first groups))
                       (conj! (conj! res cur) (first groups)))
                     []))))))))

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

(defn find-note-step
  "Find the step of a note (eg. A) given its segment and the positions
  of the staff lines"
  [head refs staves]
  ;; TODO
  ["A" 4])

(defn interpret-note
  "Convert a group of symbols to a note"
  [pre beam head post refs staves]
  (let [[step octave] (find-note-step head refs staves)
        step-with-accidental (if (= pre :sharp)
                                (str step "#")
                                (if (= pre :flat)
                                  (str step "b")
                                  ;; :natural, but we don't handle key
                                  ;; signatures so we ignore natural
                                  ;; symbols.
                                  step))
        duration (if (and
                      (= (:class head) :notehead_black)
                      (or (= beam :beam) (= beam :beam_hook)
                          (= post :flag_1) (= post :flag_1_up)))
                   ;; Here, a duration of 1 means a black note. It
                   ;; will be converted later to a duration compatible
                   ;; with MusicXML's duration.
                   1/2
                   (if (or (= post :flag_2) (= post :flag_2_up))
                     1/4
                     (if (= (:class head) :notehead_void)
                       2
                       (if (= (:class head) :whole_note)
                         4
                         ;; Defaults to black note
                         1))))]
    (->score-note step-with-accidental octave duration)))

(defn parse
  "Parse a non-terminal from the groups of note"
  [groups symbol refs staves]
  (let [rests [:quarter_rest :eighth_rest :one_16th_rest]
        accidentals [:sharp :flat :natural]
        dots [:dot_set]
        times [:common_time :cut_time :time_four :time_four_four :time_six_eight
               :time_three :time_three_four :time_two :time_two_four]
        clefs [:g_clef :g_clef_8vb :f_clef :c_clef]
        ;; Note: the _2 and _3 note heads are not handled
        noteheads [:notehead_black :notehead_black_2 :notehead_black_3
                   :notehead_void :notehead_void_2
                   :whole_note :whole_note_2]
        beams [:beam :beam_hook]
        flags [:flag_1 :flag_1_up :flag_2 :flag_2_up]
        time-default :time_four_four
        clef-default :g_clef
        notes-first (concat accidentals rests beams noteheads)]
    (case symbol
      :system (let [[clef groups'] (parse groups :clef refs staves)
                    [time groups''] (parse groups' :time refs staves)
                    [notes rest] (parse groups'' :notes refs staves)]
                [(->score-system clef time notes) rest])
      :notes (let [[note groups'] (parse groups :note refs staves)
                   [notes groups'']
                   (if (group-contains (first groups') notes-first)
                     (parse groups' :notes refs staves)
                     [[] groups'])]
               [(cons note notes) groups''])
      :note (if (group-contains (first groups) rests)
              (let [seg (group-extract (first groups) rests)]
                [(->score-note :rest 0
                               (case (:class seg)
                                 :quarter_rest 1
                                 :eighth_rest 1/2
                                 :one_16th_rest 1/4))
                 (rest groups)])
              (let [[pre _] (parse groups :pre refs staves)
                    [[beam head] _] (parse groups :note_body refs staves)
                    [post _] (parse groups :post refs staves)]
                [(interpret-note pre beam head post refs staves) (rest groups)]))
      :pre (if (group-contains (first groups) accidentals)
             (let [seg (group-extract (first groups) accidentals)]
               [(:class seg) groups])
             [:none groups])
      :post (if (group-contains (first groups) dots)
              ;; TODO: a group can contain a flag and a dot
              (let [seg (group-extract (first groups) dots)]
                [(:class seg) groups])
              (parse groups :flag refs staves))
      :note_body (let [[beam _] (parse groups :beam refs staves)
                       [notehead _] (parse groups :notehead refs staves)]
                   [[beam notehead] groups])
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
      ;; Here, we should parse the next system or measure
      (do (println "Remaining: " (first rest))
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
    [:time
     [:beats beats]
     [:beat-type beat-type]]))

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
    [:clef {:number 1}
     [:sign sign]
     [:line (str line)]]))

(defn note-to-musicxml
  [note]
  [:note
   (if (= (:step note) :rest)
     [:rest]
     [:pitch
      [:step (:step note)]
      [:octave (str (:octave note))]])
   [:duration (str (* divisions (:duration note)))]])

(defn system-to-musicxml
  "Convert a system to MusicXML data"
  [system]
  ;; Only one measure for the moment
  (into []
        (concat
         [:measure {:number "1"}
          [:attributes
           [:divisions (str divisions)]
           [:key]
           [:staves "1"]
           (clef-to-musicxml (:clef system))]]
         (map note-to-musicxml (:notes system)))))

(defn to-musicxml
  "Convert what 'interpret' computed into MusicXML"
  [score]
  [:score-partwise {:version "3.0"}
   [:part-list [:score-part {:id "P1"}]]
   [:part {:id "P1"}
    (system-to-musicxml score)]])

(defn semantic
  "Find the semantic of a score given the notes positions and classes"
  [in-classes in-refs in-staves out-xml]
  (let [segments-vectors (read-vector in-classes)
        refs (read-vector in-refs)
        staves (read-vector in-staves)
        segments (map to-classified-segment segments-vectors)
        groups (refine-groups (group-vertically segments))
        score (interpret groups refs staves)
        musicxml (to-musicxml score)]
    (spit
     out-xml
     (with-out-str
       (binding [*prxml-indent* 2]
         (prxml musicxml))))))
