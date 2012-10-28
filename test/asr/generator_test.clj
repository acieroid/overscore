(ns asr.generator-test
  (:use clojure.test
        asr.generator
        asr.musicxml))

(deftest test-generate-note
  (is (= (generate-note (->note :C4 1))
         '(play :C4 1))))

(deftest test-generate-bar
  (is (= (generate-bar (->bar
                        1
                        (list
                         (->note :C4 1)
                         (->note :A4 1)
                         (->note :G4 1)
                         (->note :C4 1))))
         '(bar
           (play-seq
            (play :C4 1)
            (play :A4 1)
            (play :G4 1)
            (play :C4 1))))))

(deftest test-generate-prog
  (is (= (generate-prog (->prog
                         "P1"
                         (list
                          (->bar
                           1
                           (list
                            (->note :C4 1))))))
         '(defprog P1
            (bar
             (play-seq
              (play :C4 1)))))))

(deftest test-generate-song
  (is (= (generate-song (->song
                         (list
                          (->prog
                           "P1"
                           (list
                            (->bar
                             1
                             (list
                              (->note :C4 1)))))))
                        'foo)
         '((defprog P1
             (bar
              (play-seq
               (play :C4 1))))
           (defsong foo
             [P1 sampled-piano])))))