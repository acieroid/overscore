(ns asr.musicxml-test
  (:use clojure.test
        asr.musicxml))

(deftest test-note-descr
  (is (= :C4
         (note-descr
          {:tag :note
           :content [{:tag :pitch
                      :content [{:tag :step :content ["C"]}
                                {:tag :octave :content ["4"]}]}]}))))

(deftest test-note-descr-alter
  (is (= :C#4
         (note-descr
          {:tag :note
           :content [{:tag :pitch
                      :content [{:tag :step :content ["C"]}
                                {:tag :octave :content ["4"]}
                                {:tag :alter :content ["1"]}]}]}))))

(defn double=
  "Test the equality of two doubles"
  [x y]
  (< (Math/abs (- x y)) 0.000001))

(deftest test-parse-note
  (let [note
        (parse-note
         {:tag :note
          :content [{:tag :pitch
                     :content [{:tag :step :content ["C"]}
                               {:tag :octave :content ["4"]}]}
                    {:tag :duration
                     :content ["1.0"]}]})]
    (is (= (:descr note) :C4))
    (is (double= (:duration note) 1.0))))