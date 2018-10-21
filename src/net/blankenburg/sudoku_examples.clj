(ns net.blankenburg.sudoku-examples
  (:use net.blankenburg.sudoku)
  (:use clojure.set))

(def STANDARD_BOARD (->BoardProperties (set (range 1 10)) 3 3 3 3))

(def canonical-board (make-canonical-board STANDARD_BOARD))

(println "The canonical, non-randomized, \"minimal\" board:")
(print-board STANDARD_BOARD canonical-board)


(defn make-and-print-random-board []
  (let [board (make-random-board STANDARD_BOARD)] 
    (do
      (print-board STANDARD_BOARD board)
      board)))
    
(println "\nA random board-1:")
(def board-1 (make-and-print-random-board)) 

(def puzzle-1  
  (let [evens (filter even? (get-character-set STANDARD_BOARD))
        nils (repeat (count evens) \_)
        replacement-map (zipmap evens nils)]
    (replace replacement-map board-1)))

(println "\nboard-1 with even numbers removed:")
(print-board STANDARD_BOARD puzzle-1 )

(println "\nSolving...")
(def solution (solve STANDARD_BOARD puzzle-1 ))

(println "\nSolution:")
(print-board STANDARD_BOARD solution)

(println "\nComputing a puzzle with max. 50 hiddens and a unique a solution...")
(def board-2 (make-random-board STANDARD_BOARD))
(def puzzle-2 (make-puzzle STANDARD_BOARD board-2 50))
(println "\nHere it is:")
(print-board STANDARD_BOARD puzzle-2)

(println "\nComputing a puzzle giving custom candidates:")
(def candidates
  (let [positions (set (range (get-size STANDARD_BOARD)))]
    (difference 
      positions
      (filter #(= 0 (mod % 4)) positions)
      (filter #(= 0 (mod % 5)) positions))))
(def board-3 (make-random-board STANDARD_BOARD))
(def puzzle-3 (last (puzzle-seq STANDARD_BOARD board-3 candidates)))
(println "\nHere it is:")
(print-board STANDARD_BOARD puzzle-3)
(println "\nMinimize it (hide characters as long as the solution is still unique)...")
(def minpuzzle-3 (minimize-puzzle STANDARD_BOARD puzzle-3))
(print-board STANDARD_BOARD minpuzzle-3)

(println "\nComputing a random minimal puzzle with unique a solution...")
(def board-4 (make-random-board STANDARD_BOARD))
(def puzzle-4 (minimize-puzzle STANDARD_BOARD board-4))
(println "\nHere it is:")
(print-board STANDARD_BOARD puzzle-4)

