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
                     :content ["1.0"]}]}
         1)]
    (is (= (:descr note) :C4))
    (is (double= (:duration note) 1/4))))

(deftest test-parse-measure
  (let [measure
        {:tag :measure
         :attrs {:number "1"}
         :content
         [{:tag :attributes
           :content
           [{:tag :divisions :content ["4"]}]}
          {:tag :note
           :content
           [{:tag :pitch
             :content [{:tag :step :content ["C"]}
                       {:tag :octave :content ["4"]}]}
            {:tag :duration :content ["1.0"]}]}]}
        bar (second (parse-measure measure))]
    (is (= (:number bar) 1))
    (is (= (:descr (first (:notes bar)))
           :C4))
    (is (double= (:duration (first (:notes bar)))
                 1/16))))