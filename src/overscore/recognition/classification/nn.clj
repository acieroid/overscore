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
    (.iteration trainer)
    (while (> (.getError trainer) err)
      (println "training, error is " (.getError trainer))
      (.iteration trainer))))

(def net
  (network
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
        dataset (dataset input
                      (map double (range (count output))))
        new-labels (reduce (fn [map [sym val]] (assoc map val sym))
                           (hash-map)
                           (map (fn [x y] [x y]) syms (range)))]
    (swap! labels (fn [_] new-labels))
    (trainer 0.05 :network net :training-set dataset)))

(defn classify-nn
  "Classify a symbol using the trained neural network"
  [^BufferedImage img segment]
  (let [input (data (resize-to-vector img segment))
        class-id (int (.getData (.compute net input) 0))
        class (get @labels class-id)]
    (if (nil? class)
      :empty
      class)))
