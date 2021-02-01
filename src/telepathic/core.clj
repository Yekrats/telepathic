(ns telepathic.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [clojure.set :as s]
						[clojure.term.colors :as term-color]))
(comment
  ;;  DONE Set of action cards.
  ;;  DONE Set up COLOR player and SHAPE player. Give each player a goal card and a lose condition card.
  ;;  DONE Function tests whether game is won based on board state & player conditions.
  ;;  DONE Function tests whether game is lost based on board state & player conditions.
  ;;  DONE Deck of action cards
  ;;  TODO 4 random displayed action cards
  ;;  DONE Pretty display of the grid (board).
  ;;  DONE EW-Do-si-do function.
  ;;  DONE NS-Do-si-do function.
  ;;  DONE EW-Reverse function.
  ;;  DONE NS-Reverse function.
  ;;  DONE Corner-clockwise function.
  ;;  DONE Corner-counterclockwise function.
  ;;
  ;;       Game sequence:
  ;;  TODO 1. Start with color player.
  ;;  TODO 2. Current player selects action card.
  ;;  TODO 3. Other player applies action card to the the game state.
  ;;  TODO 4. Check to see if the game is lost, and conclude the game if it is.
  ;;  TODO 5. Either player may make an Announcemnt.
  ;;  TODO 6. If no announcement is made, the other player begins their turn as the current player.
  ;;
  ;;  1 2 5 6 / 3 4 7 8 / 9 10 13 14 / 11 12 15 16
  ;;  (partition 2 (rest (range 17)))
  ;;  ;; => ((1 2) (3 4) (5 6) (7 8) (9 10) (11 12) (13 14) (15 16))
  )
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def players [:color-player :shape-player])

(def colors [:purple :blue :green :orange])

(def shapes [:plus :circle :star :bacon])

(def actions [:row-east :row-west :col-north :col-south
              :ew-do-si-do :ns-do-si-do :ew-reverse :ns-reverse
              :corner-clockwise :corner-counterclockwise])

(def play-state
  {:color-player
      { :win  :empty
        :lose :empty }
   :shape-player
      { :win  :empty
        :lose :empty }
   :board []
   :actions {
             :deck []
             :available []
             :discard []}
   })

(def tiles (vec (apply concat (into [] (for [color colors] (into [] (for [shape shapes] [color shape]))))))) ; added (apply concat ...) to flatten one level. -- sws

(defn condition-cards [cards]
  (zipmap [:win :lose] (take 2 (shuffle cards))))


(defn all-match?
  "Checks all the members of a set. If all match, return that value. Otherwise return nil."
  [set]
  (when (apply = set)
    (first set)))

(defn check4
  "Take in a set of 4 paired items.
  Check to see if any 3 contiguous items have a matching pattern in any color or shape.
  Returns nil if nothing found, or returns the matching color or shape."
  [set]
  (or
    (all-match? (first (apply map vector (first (partition 3 1 set)))))
    (all-match? (first (apply map vector (second (partition 3 1 set)))))
    (all-match? (second (apply map vector (first (partition 3 1 set)))))
    (all-match? (second (apply map vector (second (partition 3 1 set)))))))

;; (def perm "All the permutations of tiles." (combo/permutations tiles))

(defn rot-90
  "Takes a sequence of 16 color/shape pairs and rotates it 90°."
  [s]
  (vec (apply concat (apply mapv vector (partition 4 s)))))

(defn any-row-match?
  "Performs check4 function, taking the first row '(take 4 s)', then calling
  itself recursively until all are taken."
  [s]
  (when (seq s)
    (or (check4 (take 4 s)) (any-row-match? (drop 4 s)))))

(defn any-col-match?
  "The same as any-row-match? function, but performing a 90° rotation first,
  to capture columns."
  [s] (any-row-match? (rot-90 s)))

(defn any-rc-match?
  "Do any row or column have a matching set [s] of 3?"
  [s] ((some-fn any-row-match? any-col-match?) s))

(defn test-each-row
  "Returns sequence of matched 3s in the 4 rows"
  [s] (flatten (when (seq s)
             (conj [] (check4 (take 4 s)) (test-each-row (drop 4 s))))))

(defn push-one-row-forwards
  "Inputs a set of 4, outputs set pushed by one."
  [[%1 %2 %3 %4]]
  (seq [%4 %1 %2 %3]))

(defn push-one-row-backwards
  "Inputs a set of 4, outputs set pushed backwards by one."
  [[%1 %2 %3 %4]]
  (seq [%2 %3 %4 %1]))

(defn push-one-row-east
  "Takes in a set of 16 tiles (s), and pushes one 'rownum' to the east."
  [s rownum]
  (vec (apply concat (for [i (range 4)]
                       (if (= i rownum)
                         (push-one-row-forwards (take 4 (drop (* i 4) s)))
                         (take 4 (drop (* i 4) s)))))))

(defn push-one-row-west
  "Takes in a set of 16 tiles (s), and pushes one 'rownum' to the west."
  [s rownum]
  (vec (apply concat (for [i (range 4)]
                       (if (= i rownum)
                         (push-one-row-backwards (take 4 (drop (* i 4) s)))
                         (take 4 (drop (* i 4) s)))))))

(defn push-one-row-north   "Takes in a set of 16 tiles, and pushes one 'colnum' to the north."
  [s colnum]
  (rot-90 (push-one-row-west (rot-90 s) colnum)))

(defn push-one-row-south   "Takes in a set of 16 tiles, and pushes one 'colnum' to the south."
  [s colnum]
  (rot-90 (push-one-row-east (rot-90 s) colnum)))

(defn ew-reverse
  "Takes in a set of 16 tiles (s), and reverses one east-west row."
  [s rownum]
  (vec (apply concat (for [i (range 4)]
                       (if (= i rownum)
                         (reverse (take 4 (drop (* i 4) s)))
                         (take 4 (drop (* i 4) s)))))))

(defn ns-reverse   "Takes in a set of 16 tiles, and reverses one north-south column."
  [s colnum]
  (rot-90 (ew-reverse (rot-90 s) colnum)))

(defn do-si-do "Inputs a set of 4. Outputs the same set with each pair reversed."
  [[%1 %2 %3 %4]]
   (seq [%2 %1 %4 %3]))

(defn ew-do-si-do
  "Takes in a set of 16 tiles (s), and performs do-si-do one east-west row."
  [s rownum]
  (vec (apply concat (for [i (range 4)]
                       (if (= i rownum)
                         (do-si-do (take 4 (drop (* i 4) s)))
                         (take 4 (drop (* i 4) s)))))))

(defn ns-do-si-do  "Takes in a set of 16 tiles, and performs do-si-do on one north-south column."
  [s colnum]
  (rot-90 (ew-do-si-do (rot-90 s) colnum)))


(defn test-each-column   "Returns sequence of matched 3s in the 4 columns" [s]
  (test-each-row (rot-90 s)))

(defn test-rc "Tests each row and column. Returns a list of matched keys."
  [s]
  (remove #(nil? %) (concat (test-each-row s) (test-each-column s))))

(defn rotate-board-clockwise [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]]
  [c13 c9 c5 c1
	 c14 c10 c6 c2
	 c15 c11 c7 c3
	 c16 c12 c8 c4])

(defn rotate-quad0-clockwise [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]]
	"Rotate the tiles of the upper left quadrant clockwise."
  [c5 c1 c3 c4
	 c6 c2 c7 c8
	 c9 c10 c11 c12
	 c13 c14 c15 c16])

(defn rotate-quad0-counterclockwise [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]]
	"Rotate the tiles of the upper left quadrant counterclockwise."
	[c2 c6 c3 c4
	 c1 c5 c7 c8
	 c9 c10 c11 c12
	 c13 c14 c15 c16])

(defn rotate-quad [board n quadrant-rotation-function]
	"Rotate quadrant N of board according to QUADRANT-ROTATION-FUNCTION where quadrants are numbered
	sequentially, clockwise from upper left."
	(let [rotated-rotated (quadrant-rotation-function
												 ;; Rotate the whole board counterclockwise n times
												 (nth (iterate rotate-board-clockwise board) (mod (* n -1) 4)))]
		;; Rotate the whole board back to its original position
		(nth (iterate rotate-board-clockwise rotated-rotated) n)))

(defn put-quad-back-in "Inserts a set of q back into a particular set at quad n."
  [q s n])

(def sls                                                  ; A shuffled-legal-start for testing purposes.
  (loop [set (shuffle tiles) i 0]
    (if (or (not (any-rc-match? set)) (> i 100))
      (if (> i 99)
        i
        set)
      (recur (shuffle tiles) (inc i)))))

(defn initiate-actions []
  (let [deck (shuffle actions)]
    {:available (take 4 deck)
     :deck (nthrest deck 4)
     :discard []}))

(defn generate-play-state []
  {:color-player (condition-cards colors)
   :shape-player (condition-cards shapes)
   :board sls
   :actions (initiate-actions)
   })


(defn asset-name
  "Takes in a key-pair (color & shape). Returns the name of the asset." ; :green :bacon => "Green Bacon.png"
  [c s]
  (str (str/capitalize (name c)) " " (str/capitalize (name s)) ".png"))

(defn play-state-losing?
  "Evaluates a play state, to see if either players' lose condition is present.
  Returns true/false."
  [state]
  (not-empty (s/intersection
              #{(:lose (:color-player state)) (:lose (:shape-player state))}
              (set (test-rc (:board state))))))

(defn play-state-winning?
  "Evaluates a play state, to see if it's in a possible win condition --
  i.e., both players have win condition present. Returns true/false."
  [state]
  (let [board (test-rc (:board state))]
    (and (some #(= % (:win (:color-player state))) board)
         (some #(= % (:win (:shape-player state))) board))))

(defn display-card [card]
	(let [term-color (cond
										 (= (first card) :purple) term-color/magenta
										 (= (first card) :green) term-color/green
										 (= (first card) :blue) term-color/blue
										 (= (first card) :orange) term-color/red)
				shape (cond
								(= (second card) :plus) "+"
								(= (second card) :bacon) "〰"
								(= (second card) :star) "★"
								(= (second card) :circle) "⃝"
								)]
		(print " " (term-color shape) " ")))

(defn display-board [board]
	(doseq [row (partition 4 board)]
		(doseq [card row]
			(display-card card)) (println)))

(comment
  (def shuff1
    [[:purple :plus] [:green :bacon] [:blue :star] [:orange :circle]
     [:green :circle] [:blue :plus] [:orange :bacon] [:purple :star]
     [:blue :star] [:orange :circle] [:purple :plus] [:green :bacon]
     [:orange :bacon] [:purple :star] [:green :circle] [:blue :plus]])
  (def shuff2
    [[:purple :plus] [:green :bacon] [:purple :star] [:orange :circle]
     [:green :circle] [:purple :plus] [:orange :bacon] [:purple :star]
     [:purple :star] [:orange :circle] [:purple :plus] [:green :bacon]
     [:orange :bacon] [:purple :star] [:green :circle] [:purple :plus]]
    )

  (def sample1 [[:purple :plus] [:purple :circle] [:purple :star] [:orange :star]])

  (def samplefail [[:purple :plus] [:blue :star] [:orange :bacon] [:blue :bacon]])

  (combo/count-permutations tiles)
  ; => 20922789888000 (21 trillion)
  ; 8.2 trillion possibilities of legal starting plays
  (defn anols
    "'Approximate number of legal sets'. Used to compute an approximation as % of legal plays."
    [times-to-try]                                ; approx number of legal sets
    (loop [n times-to-try legal 0 set (shuffle tiles)]
      (if (> n 0)
        (recur (dec n) (if (any-rc-match? set) legal (inc legal)) (shuffle tiles))
        (float (/ legal times-to-try))
        )))

  (defn card-tests? [s]
    (when ((some-fn any-rc-match? test-push-north? test-push-east? test-push-south? test-push-west?) s) true))


  (def winning-state
    {:color-player {:win :green, :lose :orange},
 :shape-player {:win :plus, :lose :star},
 :board tiles})


(defn test-push-east?   "Apply 'push east' on each row, and checks any-rc-match? after push."
  [s]
  (seq (filter identity (map #(any-rc-match? (push-one-row-east s %)) (range 4)))))

(defn test-push-west? "Apply 'push west' on each row, and checks any-rc-match? after push." [s]
  (seq (filter identity (map #(any-rc-match? (push-one-row-west s %)) (range 4)))))

(defn test-push-south? "Apply 'push south' on each column, and checks any-rc-match? after push." [s]
  (seq (filter identity (map #(any-rc-match? (push-one-row-south s %)) (range 4)))))

(defn test-push-north? "Apply 'push north' on each column, and checks any-rc-match? after push." [s]
  (seq (filter identity (map #(any-rc-match? (push-one-row-north s %)) (range 4)))))

  )
