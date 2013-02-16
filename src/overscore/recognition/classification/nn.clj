;;; Classify symbols using a neural network
(ns overscore.recognition.classification.nn
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        enclog.nnets
        enclog.training)
  (:import java.awt.image.BufferedImage))

(def net
  (network (neural-pattern :feed-forward)
           :activation :sigmoid
           :input 400
           :output 1
           :hidden [400]))

(def labels (atom {}))

(defn train-network
  "Train the neural network with the training set data"
  []
  (let [input (map :data @training-set)
        output (map :class @training-set)
        syms (keys (group-by (fn [x] x) output))
        dataset (data :basic-dataset
                      input
                      (map double (range (count output))))
        trainer (trainer :back-prop :network net :training-set dataset)
        new-labels (reduce (fn [map [sym val]] (assoc map val sym))
                           (hash-map)
                           (map (fn [x y] [x y]) syms (range)))]
    (swap! labels (fn [_] new-labels))
    (try
      (train trainer 0.01 500 [])
      ;; Encog throws an exception, but the training is successfull (or is it?)
      (catch Exception e nil))))

(defn classify
  "Classify a symbol using the trained neural network"
  [^BufferedImage img segment]
  (let [input (data :basic (resize-to-vector img segment))
        class-id (int (.getData (.compute net input) 0))
        class (get @labels class-id)]
    (if (nil? class)
      :empty
      class)))
