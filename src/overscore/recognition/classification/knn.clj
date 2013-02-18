;;; Classify symbols (described by a segment of an image) using
;;; k-Nearest-Neighbours algorithm
(ns overscore.recognition.classification.knn
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        overscore.recognition.classification.hausdorff
        overscore.recognition.classification.euclidian)
  (:import java.awt.image.BufferedImage))

(defn k-min
  "Return the k minimum elements (given a key function) of a vector"
  [k key vec]
  (loop [mins (take k vec)
         vec (drop k vec)]
    (if (empty? vec)
      mins
      (if (empty? (filter #(< (key (first vec)) (key %)) mins))
        (recur mins (rest vec))
        (recur (cons (first vec) (drop 1 (sort-by (fn [x] (key x)) > mins)))
               (rest vec))))))

(defn classify-knn
  "Classify a symbol contained in a segment of an image. Returns its
  class"
  [^BufferedImage img segment & {:keys [k distance]
                                 :or {k 3
                                      distance euclidian-distance}}]
  (let [neighbours (k-min k #(distance img segment %) @training-set)]
    (if (empty? neighbours)
      ;; No neighbour (should not happen if the training set is not empty)
      :empty
      ;; Return the most frequent class within the neighbours
      (first
       (apply
        (partial max-key second)
        (frequencies
         (map :class neighbours)))))))