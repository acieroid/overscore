;;; Classify symbols using a neural network
(ns overscore.recognition.classification.nn
  (:use overscore.recognition.segmentation.segment
        overscore.recognition.classification.training
        clojure.math.numeric-tower)
  (:import java.awt.image.BufferedImage
           org.encog.neural.networks.BasicNetwork
           org.encog.neural.networks.layers.BasicLayer
           org.encog.neural.networks.training.Train
           org.encog.neural.networks.training.propagation.back.Backpropagation
           org.encog.neural.data.basic.BasicNeuralDataSet
           org.encog.neural.data.basic.BasicNeuralData
           org.encog.engine.network.activation.ActivationSigmoid))

(defn network
  "Create a neural network"
  [& {:keys [input output hidden]}]
  (let [net (BasicNetwork.)]
    (.addLayer net (BasicLayer. (ActivationSigmoid.) true input))
    (doseq [n hidden]
      (.addLayer net (BasicLayer. (ActivationSigmoid.) true n)))
    (.addLayer net (BasicLayer. (ActivationSigmoid.) true output))
    (.finalizeStructure (.getStructure net))
    (.reset net)
    net))

(defn dataset
  "Create a dataset"
  [data ideal]
  (BasicNeuralDataSet.
   (into-array (map double-array data))
   (into-array (map double-array ideal))))

(defn data
  "Create a basic data"
  [value]
  (BasicNeuralData. (double-array value)))

(defn train
  "Train a neural network until the error is less than err"
  [err & {:keys [network training-set]}]
  (let [trainer (Backpropagation. network training-set)]
    (println "starting training, until error is" err)
    (.iteration trainer)
    (while (> (.getError trainer) err)
      (println "training, error is" (.getError trainer))
      (.iteration trainer))
    (println "done training, final error:" (.getError trainer))))

(def net (atom nil))
(def labels (atom []))

(defn class-to-vector
  "Convert a certain class (eg :g_clef) to a vector that represents
  it (eg. [0 0 1 0 …])"
  [class]
  (let [index (first (keep-indexed #(when (= %2 class) %1) @labels))]
    (if index
      (concat
       (take index (repeat 0.0))
       [1.0]
       (take (- (count @labels) (inc index)) (repeat 0.0)))
      (println "Invalid class: " class))))

(defn vector-to-class
  "Convert a vector returned by the neural network to a class"
  [vector & {:keys [threshold]
             :or {threshold 0.5}}]
  (loop [vector vector
         i 0
         max-i -1
         max threshold]
    (if (empty? vector)
      (if (== max-i -1)
        :empty
        (nth @labels max-i))
      (if (> (first vector) max)
        (recur (rest vector) (inc i) i (first vector))
        (recur (rest vector) (inc i) max-i max)))))

(defn train-network
  "Train the neural network with the training set data"
  []
  (let [input (map :data @training-set)
        output (map :class @training-set)
        syms (keys (group-by (fn [x] x) output))
        _ (swap! labels (fn [_] syms))
        dataset (dataset input
                         (map class-to-vector output))]
    (swap! net (fn [_] (network :input 400 :output (count syms) :hidden [400])))
    (train 0.05 :network @net :training-set dataset)))

(defn classify-nn
  "Classify a symbol using the trained neural network"
  [^BufferedImage img segment]
  (let [input (data (resize-to-vector img segment))
        vector (into [] (.getData (.compute @net input)))
        class (vector-to-class vector)]
    class))
