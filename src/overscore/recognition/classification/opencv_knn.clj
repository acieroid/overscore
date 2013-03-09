;;; Classify symbols using OpenCV's kNN
(ns overscore.recognition.classification.opencv-knn
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        clojure.java.io)
  (:import java.awt.image.BufferedImage
           java.io.OutputStreamWriter
           java.io.InputStreamReader
           java.io.PushbackReader
           java.io.File))

(def labels (atom []))
(def process (atom nil))
(def out-stream (atom nil))
(def in-stream (atom nil))

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
  "Create the OpenCV classifier subprocess"
  [& {:keys [k]
      :or {k 3}}]
  (if (empty? @training-set)
    (println "Training set is empty, not creating OpenCV subprocess")
    (let [p (.exec (Runtime/getRuntime) "./src/overscore/recognition/classification/opencv_knn")
          stdin (PushbackReader. (InputStreamReader. (.getInputStream p)))
          stdout (OutputStreamWriter. (.getOutputStream p))]
      (create-labels @training-set)
      (swap! process (fn [_] p))
      (swap! out-stream (fn [_] stdout))
      (swap! in-stream (fn [_] stdin))
      ;; Write the training set
      (.write stdout (str k " "))
      (.write stdout (str (count @training-set) " "))
      (.write stdout "400")
      (.write stdout "\n")
      (doseq [elem @training-set]
        (.write stdout (str (class-to-number (:class elem))))
        (.write stdout "\n")
        (doseq [bit (:data elem)]
          (.write stdout (str bit " ")))
        (.write stdout "\n"))
      (.flush stdout))))

(defn classify-opencv-knn
  "Classify a symbol using OpenCV's kNN"
  [^BufferedImage img segment]
  (let [vec (resize-to-vector img segment)]
    ;; Write the data to OpenCV's process input stream
    (doseq [bit vec]
      (.write @out-stream (str bit " ")))
    (.write @out-stream "\n")
    (.flush @out-stream)
    ;; Read the response
    (let [response (read @in-stream)]
      (number-to-class response))))
