;;; Implement utilities to read and write overscore data to files
(ns overscore.tools.files
  (:use clojure.java.io)
  (:import java.io.File
           java.io.PushbackReader))

(defn read-vector
  "Safely read a vector from a file"
  [file]
  (with-open [f (PushbackReader. (reader file))]
    (binding [*read-eval* false]
      (read f))))

(defn write-vector
  "Write a vector to a file"
  [file vec]
  (with-open [f (writer file)]
      (.write f (str vec))))