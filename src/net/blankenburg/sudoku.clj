(ns net.blankenburg.sudoku
  (:use clojure.set)
  (:use clojure.test)
  (:use net.blankenburg.backtrack))

(def HIDDEN_CHARACTER \_)

(defprotocol BoardPropertiesProtocol
  "Defines methods for a Sudoku board."
  (get-character-set [this] "Gets the set of characters")
  (get-width [this] "Gets board width in single cells.")
  (get-height [this] "Gets board height in single cells.")
  (get-x [this k] "Gets the x-coordinate (cells from left) of cell number k")
  (get-y [this k] "Gets the y-coordinate (cells from top) of cell number k")
  (get-quadrant-width [this] "Gets quadrant width in single cells.")
  (get-quadrant-height [this] "Gets quadrant height in single cells.")
  (get-quadrant-x [this k] "Gets the quadrant x-coordinate (quadrants from left) of cell number k")
  (get-quadrant-y [this k] "Gets the quadrant x-coordinate (quadrants from left) of cell number k")
  (get-size [this] "Gets the number of cells in the board"))

; Define an actual record representing board properties
(defrecord BoardProperties [character-set quadrant-width quadrant-height x-quadrants y-quadrants]
  BoardPropertiesProtocol
    (get-character-set [this] character-set)
    (get-width [this] (* quadrant-width x-quadrants))
    (get-height [this] (* quadrant-height y-quadrants))
    (get-x [this k] (mod k (get-width this)))
    (get-y [this k] (int (/ k (get-width this)))) 
    (get-quadrant-width  [this] quadrant-width)
    (get-quadrant-height [this] quadrant-height)
    (get-quadrant-x [this k] (int (/ (get-x this k) quadrant-width)))
    (get-quadrant-y [this k] (int (/ (get-y this k) quadrant-height)))
    (get-size [this] (* (get-width this) (get-height this))))

(defn partition-by-rows 
  "Paritions a board into a sequence of rows."
  [board-properties board]
  (let [width (get-width board-properties)] 
    (partition width width nil board)))

(defn get-row 
  "Gets the row (as character sequence) of the character at position k."
  [board-properties board k]
  (nth (partition-by-rows board-properties board) (get-y board-properties k) nil))

(defn get-column 
  "Gets the column (as character sequence) of the character at position k."
  [board-properties board k]
  (map #(nth % (get-x board-properties k) nil) (partition-by-rows board-properties board)))

(defn get-quadrant 
  "Gets the quadrant (as character sequence) of the character at position k."
  [board-properties board k] 
  (let [qw (get-quadrant-width board-properties) 
        qh (get-quadrant-height board-properties) 
        qx (get-quadrant-x board-properties k) 
        qy (get-quadrant-y board-properties k) 
        min-x (* qx qw) 
        min-y (* qy qh)
        rows (partition-by-rows board-properties board)] 
    (flatten (map #(take qw (drop min-x %)) (take qh (drop min-y rows)))))) 

(defn make-candidate-fn   
  "Creates a function which, given the previously constructed cells, gets all possible candidates for the next cell." 
  [board-properties]
  (letfn 
    [(candidate-fn
       ([cells]
         (candidate-fn cells (count cells)))
       ([cells k] 
         (difference 
           (get-character-set board-properties)
           (set (get-row board-properties cells k))
           (set (get-column board-properties cells k))
           (set (get-quadrant board-properties cells k)))))]
    candidate-fn))

(defn make-canonical-board
  "Creates the \"canonical\" board, i.e. the board using a non-shuffled candidate-fn."
  [board-properties]  
  (net.blankenburg.backtrack/construct-seq-backtracking 
    (get-size board-properties)
    (make-candidate-fn board-properties)))

(defn make-random-board 
  "Creates a random board with given properties."
  [board-properties]
  (construct-seq-backtracking (get-size board-properties) (comp shuffle (make-candidate-fn board-properties)))) 

(defn candidate-fn-with-givens
  "Given a candidate-fn and a vector of givens, yields a new candidate-fn respecting those givens.
A \"given\" is any character in the character set defined by board-properties. A \"hidden\" is any
other character."
  [board-properties candidate-fn givens]
  (fn [cells]
    (let [k (count cells)
          given-el (nth givens k nil)]
      (if (contains? (get-character-set board-properties) given-el)
        [given-el]
        (candidate-fn (into cells (drop k givens)) k)))))

(defn solve
  "Solves the given sudoku defined by board."
  ([board-properties board]
    (solve board-properties board (make-candidate-fn board-properties)))
  ([board-properties board candidate-fn]
    (let [solving-candidate-fn (candidate-fn-with-givens board-properties candidate-fn board)]
      (construct-seq-backtracking (get-size board-properties) solving-candidate-fn))))

(defn has-unique-solution? 
  "Tests if the given givens have a unique solution."
  [board-properties givens]
  (let [candidate-fn (make-candidate-fn board-properties)
        reverse-fn (comp reverse candidate-fn)
        solution1 (solve board-properties givens candidate-fn)
        solution2 (solve board-properties givens reverse-fn)]
    (= solution1 solution2)))
 
(defn hide 
  "Returns a new board with the character at position k hidden."
  [board k]
  (assoc board k HIDDEN_CHARACTER))

(defn given? 
  "Tests whether the character at position k is given."
  [board-properties board k]
  (contains? (get-character-set board-properties) (nth board k)))

(defn given-positions 
  "Returns a sequence of the positions of all givens in the board."
  [board-properties board]
  (filter 
    #(given? board-properties board %) 
    (range (count board))))
                             
(defn drop-until-proper-candidate 
  "Given a sequence of candidates, drops all from the start whose hiding does not yield proper boards."
  [board-properties board candidates]
  (drop-while 
    #(not (has-unique-solution? board-properties (hide board %))) 
    candidates))

(defn hide-next
  "Given a pair of a board and a sequence of candidates, hides the first one yielding a proper board. 
A tuple of the given board-properties, the resulting board and the remaining candidates is returned, 
or nil if no suited candidate remains."
  [[board-properties board candidates]]
    (let [new-candidates (drop-until-proper-candidate board-properties board candidates)]
      (when (seq new-candidates)
        (let [new-board (hide board (first new-candidates))]
          [board-properties new-board (rest new-candidates)]))))

(defn puzzle-seq 
  "Returns a lazy sequence of boards, each one with one more element hidden until none more can be hidden
such that there is a unique solution. If candidates is given, they are tried to hide in order. Otherwise,
a random candidate sequence is used."
  ([board-properties board]
    (puzzle-seq board-properties board (shuffle (given-positions board-properties board))))
  ([board-properties board candidates]
      (map second (take-while seq (iterate hide-next [board-properties board candidates])))))

(defn make-puzzle 
  "Creates a proper sudoku puzzle with max. n hiddens."
  [board-properties board n]
  (first (drop (dec n) (puzzle-seq board-properties board))))

(defn minimize-puzzle 
  "Creates a proper sudoku puzzle where no given can be hidden unless loosing uniqueness of solution."
  [board-properties board]
  (last (puzzle-seq board-properties board)))

(defn print-board 
  "Prints a board." 
  [board-properties board]
  (do 
    (doseq [row (partition-by-rows board-properties board)] 
      (print \|)
      (doseq [character row] 
        (print (str character "|"))) 
      (println))
    (print (str "Givens: " (count (given-positions board-properties board))))
    (println (str " Proper: " (if (has-unique-solution? board-properties board ) "yes" "no")))))
  



