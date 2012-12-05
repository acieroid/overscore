;;; Identify the position of the staffline using a method described by
;;; Fujinaga in 1988.

;;; The method is done in two times: first we locate
;;; the systems, then for each system we locate the stafflines.

;;; To locate the systems, we do a y-projection over the entire image,
;;; and look for 5 peaks of black pixels, separated from a similar
;;; distance.

;;; Once the systems are located, for each system we look for the
;;; beginning and the end of the stafflines by moving "windows"
;;; horizontally and doing y-projections of the pixels located on
;;; those windows.
(ns overscore.staffline.identification
  (:use overscore.proj
        overscore.utils)
  (:import java.awt.image.BufferedImage))

(defn diff
  "Approximate the derivative of a function, given its values at certain points"
  [coll]
  (loop [l coll
         acc []]
    (if (empty? (rest (rest l)))
      ;; Less than 3 elements
      (reverse acc)
      ;; Add (f(x+1)-f(x-1))/2 to acc, with (first l) = f(x-1)
      (recur (rest l)
             (cons (/ (- (nth l 2) (first l)) 2) acc)))))

(defn find-maximas
  "Find the maximas among a collection, which are greater than a
  certain threshold t. Similar to Fujinaga's zero_max. Return the
  indexes of those maximas"
  [coll t]
  (loop [df (diff coll)
         ;; Drop the first value of coll, because df starts with the value for
         ;; the second element of coll (since it refers to f(x-1))
         l (rest coll)
         i 1
         acc []]
    (if (empty? (rest df))
      (reverse acc)
      (recur (rest df) (rest l) (inc i)
             (if (and (>= (first df) 0) (< (second df) 0) ; we have a maxima
                      (> (first l) t)) ; and it is greater than the threshold
               (cons i acc) ; save this index
               acc)))))

(defn find-systems
  "Find the positions of the systems given the position of the
  peaks. The systems are found by looking for clusters of five or more
  peaks. Return a list of the start and end position of each system."
  [maximas]
  ;; t is the threshold value for the distance between systems
  (let [t (mean (map - (rest maximas) maximas))]
    (loop [l maximas
           acc []
           start (first l)] ; the start position of the current system
      (if (empty? (rest l))
        (reverse (cons [start (first l)] acc))
        (if (> (- (second l) (first l)) t)
          (if (== start (first l))
            (recur (rest l) acc (second l)) ; ignore empty-height clusters
            (recur (rest l) (cons [start (first l)] acc) (second l)))
          (recur (rest l) acc start))))))

(defn isolate-systems
  "From an image, isolates the system and returns a list of image,
  each image containing only one system."
  [^BufferedImage img]
  (map
   ;; Build the fragment image
   (fn [[start end]]
     (let [fragment (BufferedImage. (.getWidth img) (- end start)
                                    BufferedImage/TYPE_BYTE_BINARY)]
       (doseq [x (range (.getWidth img))
               y (range start end)]
         (.setRGB fragment x (- y start)
                  (.getRGB img x y)))
       fragment))
   ;; Find the systems positions
   (let [data (projection img :y)]
     (debug (find-systems (find-maximas data (mean data)))))))