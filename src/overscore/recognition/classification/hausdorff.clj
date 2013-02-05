;;; Compute the Hausdorff distance between two images
(ns overscore.recognition.classification.hausdorff
  (:use overscore.recognition.segmentation.segment)
  (:import java.awt.image.BufferedImage))

(defn hausdorff-distance
  "Compute the undirected Hausdorff distance between two image. The
  template image is an element from the training set, and the other
  image is represented by a (L2) segment of an image"
  [^BufferedImage image segment template]
  (let [tw (.getWidth template)
        th (.getHeight template)
        tf #(.getRGB template %1 %2)
        sw (segment-width segment)
        sh (segment-height segment)
        sf #(.getRGB image
                     (+ (:start-x segment) %1)
                     (+ (:start-y segment) %2))]
    (max
     (directed-hausdorff-distance tw th tf sw sh sf)
     (directed-hausdorff-distance sw sh sf tw th tf))))

(defrecord point [x y])

(defn point-distance
  "Compute the (squared) distance between two points"
  [a b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))]
    (+ (* dx dx) (* dy dy))))

(defn black-points
  "Return the list of black points in an image (described by its
  width, height and a function that return the color of a given pixel"
  [w h f]
  (loop [x 0
         y 0
         res (transient [])]
    (if (< y h)
      (if (< x w)
        (recur (inc x) (inc y)
               (if (not (== (f x y)) -1)
                 (conj! res (->point x y))
                 res))
        (recur 0 (inc y) res))
      (persistent! res))))

(defn directed-hausdorff-distance
  "Compute the directed Hausdorff distance between two images, given
  their size (width, height) and a function that returns a pixel value
  given the coordinates in the image"
  [aw ah af bw bh bf]
  (reduce max
          (map (reduce min
                       (map point-distance
                            (black-points bw bh bf)))
               (black-points aw ah af))))