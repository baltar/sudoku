(ns net.blankenburg.backtrack
  (:use clojure.test))

(defn construct-seq-backtracking
  "A generic backtracking algorithm generating a vector. It takes a number n of elements to produce and a function
get-candidates-fn which must accept one parameter, the sequence of previously constructed elements, and produce a sequence
of candidates for the next element."
  [n get-candidates-fn]
  (loop [constructed [] ; so far constructed elements
         previous-candidates '() ; a sequence of sequences of remaining candidates for previous elements
         next-candidates (get-candidates-fn constructed)] ; candidates for the next element
    (if (== n (count constructed))
      constructed ; if we are finished, return constructed coll
      (if-let [candidate (first next-candidates)] ; otherwise, check if we have a candidate
        (let [next-constructed (conj constructed candidate)] ; if so, append it to the constructed coll and recur to get next
          (recur next-constructed (conj previous-candidates (rest next-candidates)) (get-candidates-fn next-constructed)))
        (when (seq constructed) ; othewise, backtrack one element
            (recur (vec (butlast constructed)) (rest previous-candidates) (first previous-candidates)))))))

(deftest test-construct-seq-backtracking ; define some tests, run with (run-tests) or (test-construct-seq-backtracking)
  (letfn [(no-three [coll]
                    (if-let [last (last coll)]
                      (when (not= 3 last) ; if last element != 3, last + 1 and last + 2 as candidates. Otherwise return nil.
                        [(inc last) (+ last 2)])
                      [0]))]
    (is (= [0 1 2 4 5] (construct-seq-backtracking 5 no-three)))))

(deftest test-construct-seq-backtracking-2
  (letfn [(no-three [coll]
                    (if-let [last (last coll)]
                      (when (even? last) ; if last element even, last + 1 and last + 2 as candidates. Otherwise return nil.
                        [(inc last) (+ last 2)])
                      [0]))]
    (is (= [0 1 2 4 5] (construct-seq-backtracking 5 no-three)))))
