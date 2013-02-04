;;; Detect note heads in a level-0 segment by looking at vertical black runs
(ns overscore.recognition.segmentation.notehead
  (:use overscore.proj
        overscore.utils
        overscore.recognition.segmentation.segment)
  (:import java.awt.image.BufferedImage))

;; The different factors that are used by the method
(def nwf 0.5)
(def n-factor 6)
(def d-factor 100) ; don't use an upper bound, else the stems are not included

(defn column-black-runs
  "Return the size of black pxiels runs for a column of an image"
  [^BufferedImage img x]
  (loop [runs (transient [])
         count 0
         y 0]
    (if (< y (.getHeight img))
      (if (not (== (.getRGB img x y) -1))
        (recur runs (inc count) (inc y))
        ;; potentially new run
        (recur (if (> count 0)
                 (conj! runs count)
                 runs)
               0 (inc y)))
      (persistent! (conj! runs count)))))

(defn column-has-run
  "Check if a certain column of a L0 segment has a black run that is:
     - smaller than d-factor * d; and
     - bigger than n-factor * n

   If it is the case, return true, else false"
  [^BufferedImage img segment column d n]
  (let [runs (column-black-runs img (+ (:start-x segment) column))]
    (some #(and (>= % (* n-factor n))
                (<= % (* d-factor d)))
          runs)))

(defn column-max-run
  "Like column-has-run, but return the maximum run that satisfies the
  conditions given in column-has-run"
  [^BufferedImage img segment column d n]
  (let [runs (column-black-runs img (+ (:start-x segment) column))
        filtered (filter #(and (>= % (* n-factor n))
                               (<= % (* d-factor d)))
                         runs)]
    (if (empty? filtered)
      0
      (apply max filtered))))

(defn has-note
  "Check if a L0 segment contains a notehead and return true if it is
  the case. Does so by trying to find a segment of columns having the
  black runs we want such that the length of the segment is at least
  d*nwf where nwf is the note width factor."
  [^BufferedImage img
   segment d n]
  (let [runs (reverse
              (second
               (reduce (fn [[count runs] x]
                         (if x
                           [(inc count) runs]
                           [0 (cons count runs)]))
                       [0 []]
                       (map #(column-has-run img segment % d n)
                            (range (segment-width segment))))))]
    (some #(>= % (* nwf d)) runs)))

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
                 (if (>= run-length (* nwf d))
                   ;; Keep this run
                   (concat! result run)
                   ;; Set this run to 0
                   (concat! result (take run-length (repeat 0))))))))))

(defn detect-notes
  "Similar to has-note and notes-graph, but return the start and end
  positions of the note heads in a segment structure"
  [^BufferedImage img segment d n]
  (loop [columns (map #(column-max-run img segment % d n)
                      (range (segment-width segment)))
         i (:start-x segment)
         result (transient [])]
    (if (empty? columns)
      (persistent! result)
      (if (== (first columns) 0)
        (recur (rest columns) (inc i) result)
        (let [run-length (count (take-while #(not (zero? %)) columns))]
          (recur (drop run-length columns)
                 (+ i run-length)
                 (if (>= run-length (* nwf d))
                   (conj! result (->segment i (+ i run-length)
                                            0 (dec (.getHeight img))))
                   result)))))))