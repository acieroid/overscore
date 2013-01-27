;;; Do the level-1 segmentation
;;; To view the segments on a system:
;;; (color-segments
;;;  img
;;;  (apply concat
;;;         (map #(create-level1-segments %(detect-notes img % 16 8)) segs))
;;;  :other-color 0xFFAAAA)
(ns overscore.segmentation.level1
  (:use overscore.segmentation.level0))

(defn create-level1-segments
  "Given a segment and the list of note heads it contains, return a
  list on L1 segments"
  [segment heads]
  (loop [heads heads
         segments (transient [])
         last-end (dec (:start segment))]
    (if (empty? heads)
      (persistent! segments)
      (let [head (first heads)]
        (recur (rest heads)
               (if (== last-end (:start head))
                 (conj! segments head)
                 (conj!
                  (conj! segments
                         (->segment (inc last-end) (dec (:start head))))
                  head))
               (:end head))))))
