;;; Do the level-1 segmentation
;;; To view the segments on a system:
;;; (color-segments
;;;  img
;;;  (apply concat
;;;         (map #(create-level1-segments %(detect-notes img % 16 8))
;;;         (level0-segments img false))
;;;  :other-color 0xFFAAAA)
(ns overscore.segmentation.level1
  (:use overscore.segmentation.segment)
  (:import java.awt.image.BufferedImage))

(defn create-level1-segments
  "Given a segment and the list of note heads it contains, return a
  list on L1 segments"
  [^BufferedImage img segment heads]
  (if (empty? heads)
    []
    (loop [heads heads
           segments (transient [])
           last-end (dec (:start-x segment))]
      (if (empty? heads)
        (persistent! segments)
        (let [head (first heads)]
          (recur (rest heads)
                 (if (== last-end (:start-x head))
                   (conj! segments head)
                   (conj!
                    (conj! segments
                           (->segment (inc last-end) (dec (:start-x head))
                                      0 (dec (.getHeight img))))
                    head))
                 (:end-x head)))))))
