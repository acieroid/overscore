;;; Detect note heads in a level-0 segment by looking at vertical black runs
(ns overscore.segmentation.notehead
  (:use overscore.proj
        overscore.utils
        overscore.preprocessing.rle
        overscore.segmentation.level0)
  (:import java.awt.image.BufferedImage))

(defn column-has-run
  "Check if a certain column of a L0 segment has a black run that is:
     - smaller than 3/2 * d; and
     - bigger than 2*n

   If it is the case, return true, else false"
  [^BufferedImage img segment column d n]
  (let [rle (column-rle img (+ (:start segment) column))]
    (some #(and (>= % (* 2 n))
                (<= % (* 3/2 d)))
          rle)))

(defn column-max-run
  "Like column-has-run, but return the maximum run that satisfies the
  conditions given in column-has-run"
  [^BufferedImage img segment column d n]
  (let [rle (column-rle img (+ (:start segment) column))
        filtered (filter #(and (>= % (* 2 n))
                               (<= % (* 3/2 d)))
                         rle)]
    (if (empty? filtered)
      0
      (apply max filtered))))

(defn has-note
  "Check if a L0 segment contains a notehead and return true if it is
  the case. Does so by trying to find a segment of columns having the
  black runs we want such that the length of the segment is at least d/2"
  [^BufferedImage img segment d n]
  (let [rle (reverse
             (second
              (reduce (fn [[count rle] x]
                        (println count rle)
                        (if x
                          [(inc count) rle]
                          [0 (cons count rle)]))
                      [0 []]
                      (map #(column-has-run img segment % d n)
                           (range (segment-width segment))))))]
    (some #(>= % (/ d 2)) rle)))