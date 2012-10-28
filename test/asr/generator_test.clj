(ns asr.generator-test
  (:use clojure.test
        asr.generator
        asr.musicxml))

(deftest test-generate-note
  (= (generate-note (->note :C4 1))
     '(play :C4 1)))