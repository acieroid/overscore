;;; Classify symbols using OpenCV's kNN
(ns overscore.recognition.classification.opencv-knn
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training)
  (:import java.awt.image.BufferedImage
           org.opencv.core.CvType
           org.opencv.core.Mat
           org.opencv.ml.CvKNearest))

(def labels (atom []))
(def knn-obj (atom nil))

(defn create-labels
  "Create the labels needed by the neural network"
  [training-set]
  (let [syms (keys (group-by (fn [x] x) (map :class training-set)))]
    (swap! labels (fn [_] syms))))

(defn class-to-number
  "Convert a certain class (eg :g_clef) to a number that describes it"
  [class]
  (first (keep-indexed #(when (= %2 class) %1) @labels)))

(defn number-to-class
  "Convert a number that describes a class to the corresponding class"
  [n]
  (nth @labels n))

(defn create-knn
  "Convert the training set to OpenCV matrices and return the kNN
    object used to do classification"
  []
  (try
    ;; Load the lib if not already loaded
    ;; TODO: there's still some UnsatisfiedLinkError when trying to create a Mat
    (System/loadLibrary "opencv_java")
    (catch UnsatisfiedLinkError e))
  (let [training-vectors (Mat. (count @training-set) 400 CvType/CV_32FC1)
        training-labels (Mat. (count @training-set) 1 CvType/CV_32FC1)]
    (loop [training-set @training-set
           i 0]
      (when (not (empty? training-set))
        (let [vec (:data (first training-set))
              class (:class (first training-set))]
          (.put training-vectors i 0 (int-array vec))
          (.put training-labels i 0 (class-to-number class))
          (recur (rest training-set) (inc i)))))
    (swap! knn-obj (fn [_] (.CvKNearest training-vectors training-labels)))))

(defn classify-opencv-knn
  "Classify a symbol using OpenCV's kNN"
  [^BufferedImage img segment & {:keys [k]
                                 :or {k 3}}]
  (let [vec (resize-to-vector img segment)
        input (.Mat 1 400 CvType/CV_32FC1)
        _ (.put input 0 0 (int-array vec))
        output (.Mapt 1 1 CvType/CV_32FC1)
        vect (.find_nearest @knn-obj input k output)]
    output                              ; TODO
    ))
