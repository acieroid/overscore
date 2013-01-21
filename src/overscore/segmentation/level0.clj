;;; Do the level-0 segmentation by finding sequence of connected black
;;; regions on the x-projection of a system (as in OpenOMR).
(ns overscore.segmentation.level0
  (:use overscore.proj
        overscore.utils)
  (:import java.awt.image.BufferedImage))

(defn segments
  "Find the segments of black regions in a x-projection of an
  image. Return a list of pairs containing the start and end position
  of each segment"
  [proj]
  (loop [proj proj
         i 0
         segments []
         start -1]
    (if (empty? proj)
      (reverse segments)
      (if (> (first proj) 0)
        ;; this column has black pixels
        (recur (rest proj) (inc i) segments
               (if (= start -1)
                 i ; new region
                 start))
        ;; no black pixels
        (if (= start -1)
          (recur (rest proj) (inc i) segments start)
          ;; end of region
          (recur (rest proj) (inc i)
                 (cons [start i] segments)
                 -1))))))