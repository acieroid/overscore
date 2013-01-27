;;; Do the level-1 segmentation
(ns overscore.segmentation.level1
  (:use overscore.segmentation.level0))

(defn create-level1-segments
  "Given a list of note heads, return a list on L1 segments"
  [heads]
  (loop [heads heads
         segments (transient [])
         last-end -1]
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
