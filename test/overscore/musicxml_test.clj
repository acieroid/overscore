(ns overscore.musicxml-test
  (:use clojure.test
        overscore.musicxml))

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

(deftest test-parse-note
  (let [note
        (parse-note
         {:tag :note
          :content [{:tag :pitch
                     :content [{:tag :step :content ["C"]}
                               {:tag :octave :content ["4"]}]}
                    {:tag :duration
                     :content ["1"]}]}
         4)]
    (is (= (:descr note) :C4))
    (is (= (:duration note) 1/4))))

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
            {:tag :duration :content ["1"]}]}
          {:tag :note
           :content
           [{:tag :pitch
             :content [{:tag :step :content ["G"]}
                       {:tag :octave :content ["3"]}]}
            {:tag :duration :content ["4"]}]}]}
        bar (second (parse-measure measure))]
    (is (= (:number bar) 1))
    (is (= (first (:notes bar))
           (->note :C4 1/4)))
    (is (= (second (:notes bar))
           (->note :G3 1)))))

(deftest test-parse-measure-chord-2-notes
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
            {:tag :duration :content ["1"]}]}
          {:tag :note
           :content
           [{:tag :chord}
            {:tag :pitch
             :content [{:tag :step :content ["G"]}
                       {:tag :octave :content ["3"]}]}
            {:tag :duration :content ["4"]}]}]}
        bar (second (parse-measure measure))]
    (is (= (:number bar) 1))
    (is (= (first (:notes bar))
           (->chord [(->note :C4 1/4)
                     (->note :G3 1)])))))

(deftest test-parse-measure-chord-3-notes
  (let [measure
        {:tag :measure
         :attrs {:number "1"}
         :content
         [{:tag :attributes
           :content
           [{:tag :divisions :content ["1"]}]}
          {:tag :note
           :content
           [{:tag :pitch
             :content [{:tag :step :content ["C"]}
                       {:tag :octave :content ["4"]}]}
            {:tag :duration :content ["1"]}]}
          {:tag :note
           :content
           [{:tag :chord}
            {:tag :pitch
             :content [{:tag :step :content ["A"]}
                       {:tag :octave :content ["4"]}]}
            {:tag :duration :content ["1"]}]}
          {:tag :note
           :content
           [{:tag :chord}
            {:tag :pitch
             :content [{:tag :step :content ["G"]}
                       {:tag :octave :content ["4"]}]}
            {:tag :duration :content ["1"]}]}]}
        bar (second (parse-measure measure))]
    (is (= (:number bar) 1))
    (is (= (first (:notes bar))
           (->chord [(->note :C4 1)
                     (->note :A4 1)
                     (->note :G4 1)])))))

(deftest test-add-to-chord-and-reverse
  (is (= (-> nil
             (add-to-chord (->note :C4 1))
             (add-to-chord (->note :A4 1))
             (add-to-chord (->note :G4 1))
             (reverse-chord))
         (->chord [(->note :C4 1)
                   (->note :A4 1)
                   (->note :G4 1)]))))

(deftest test-add-to-chord-and-reverse2
  (is (= (-> (->note :C4 1)
             (add-to-chord (->note :A4 1))
             (add-to-chord (->note :G4 1))
             (reverse-chord))
         (->chord [(->note :C4 1)
                   (->note :A4 1)
                   (->note :G4 1)]))))