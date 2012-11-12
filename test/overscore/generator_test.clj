(ns overscore.generator-test
  (:use clojure.test
        overscore.generator
        overscore.musicxml))

(deftest test-generate-note
  (is (= (generate-note (->note :C4 1))
         '(play :C4 1))))

(deftest test-generate-note-chord
  (is (= (generate-note (->chord
                         [(->note :C4 1)
                          (->note :A4 1)
                          (->note :G4 1)]))
         '(play-chord
           (play :C4 1)
           (play :A4 1)
           (play :G4 1)))))


(deftest test-generate-bar
  (is (= (generate-bar (->bar
                        1 nil
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

(deftest test-generate-bar-with-voices
  (is (= (generate-bar
          (->bar
           1 nil
           [(->chord
             [(->note-seq [(->note :F4 1)
                           (->note :D4 1)
                           (->note :F4 1)])
              (->note-seq [(->note :F5 1)
                           (->note :D5 1)
                           (->note :F5 1)])])]))
         '(bar
           (play-chord
            (play-seq
             (play :F4 1)
             (play :D4 1)
             (play :F4 1))
            (play-seq
             (play :F5 1)
             (play :D5 1)
             (play :F5 1)))))))

(deftest test-generate-prog
  (is (= (generate-prog (->prog
                         "P1"
                         (list
                          (->bar
                           1 nil
                           (list
                            (->note :C4 1))))))
         '(defprog P1
            (bar
             (play :C4 1))))))

(deftest test-generate-song
  (is (= (generate-song (->song [4 4] 80
                         (list
                          (->prog
                           "P1"
                           (list
                            (->bar
                             1 nil
                             (list
                              (->note :C4 1)))))))
                        'foo)
         '((defprog P1
             (bar
              (play :C4 1)))
           (defsong foo
             {:time-signature [4 4]
              :tempo 80}
             [P1 sampled-piano])))))
