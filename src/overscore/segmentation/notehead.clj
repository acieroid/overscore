;;; Detect note heads in a level-0 segment by looking at vertical black runs
(ns overscore.segmentation.notehead
  (:use overscore.proj
        overscore.utils
        overscore.preprocessing.rle
        overscore.segmentation.level0)
  (:import java.awt.image.BufferedImage))

;; TODO: in practice, it works better with 4n instead of n given as argument
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
                        (if x
                          [(inc count) rle]
                          [0 (cons count rle)]))
                      [0 []]
                      (map #(column-has-run img segment % d n)
                           (range (segment-width segment))))))]
    (some #(>= % (/ d 2)) rle)))

(defn concat!
  "Concatenate a transient with a persirstent seq"
  [x y]
  (reduce conj! x y))

(defn notes-graph
  "Return a vector containing non-zeros where notes can be
  found. Mainly used for debugging"
  [^BufferedImage img segment d n]
  (loop [columns (map #(column-max-run img segment % d n)
                      (range (segment-width segment)))
         result (transient [])]
    (if (empty? columns)
      (persistent! result)
      (if (== (first columns) 0)
        (recur (rest columns) (conj! result 0))
        (let [run (take-while #(not (zero? %)) columns)
              run-length (count run)]
          (recur (drop run-length columns)
                 (if (>= run-length (/ d 2))
                   ;; Keep this run
                   (concat! result run)
                   ;; Set this run to 0
                   (concat! result (take run-length (repeat 0))))))))))

(defn detect-notes
  "Similar to has-note and notes-graph, but return the start and end
  positions of the note heads"
  [^BufferedImage img segment d n]
  (loop [columns (map #(column-max-run img segment % d n)
                      (range (segment-width segment)))
         i 0
         result (transient [])]
    (if (empty? columns)
      (persistent! result)
      (if (== (first columns) 0)
        (recur (rest columns) (inc i) result)
        (let [run-length (count (take-while #(not (zero? %)) columns))]
          (recur (drop run-length columns)
                 (+ i run-length)
                 (if (>= run-length (/ d 2))
                   (conj! result [i (+ i run-length)])
                   result)))))))