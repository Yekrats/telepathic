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
  ;;  DONE 4 random displayed action cards
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
  ;;  TODO 6. Is the deck empty? If so, the game is lost. Otherwise draw new action.
  ;;  TODO 7. If no announcement is made, the other player begins their turn as the current player.
  ;;
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

(defn rotate-quad0-clockwise
  "Rotate the tiles of the upper left quadrant clockwise."
  [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]]
  [c5 c1 c3 c4
	 c6 c2 c7 c8
	 c9 c10 c11 c12
	 c13 c14 c15 c16])

(defn rotate-quad0-counterclockwise
  "Rotate the tiles of the upper left quadrant counterclockwise."
  [[c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]]
	[c2 c6 c3 c4
	 c1 c5 c7 c8
	 c9 c10 c11 c12
	 c13 c14 c15 c16])

(defn rotate-quad
  "Rotate quadrant N of board according to QUADRANT-ROTATION-FUNCTION where quadrants are numbered
  sequentially, clockwise from upper left."
  [board n quadrant-rotation-function]

	(let [rotated-rotated (quadrant-rotation-function
	;; Rotate the whole board counterclockwise n times
	                       (nth (iterate rotate-board-clockwise board) (mod (* n -1) 4)))]
	;; Rotate the whole board back to its original position
	  (nth (iterate rotate-board-clockwise rotated-rotated) n)))

(defn sls []                                                 ; A shuffled-legal-start for testing purposes.
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
   :board (sls)
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
                    (= (second card) :bacon) "⌇"
                    (= (second card) :star) "★"
                    (= (second card) :circle) "⃝")]
      (print " " (term-color shape) " ")))

(defn display-state [state]
	(doseq [row (partition 4 (:board state))]
		(doseq [card row]
			(display-card card)) (println))
	(println "Available actions: " (map name (:available (:actions state)))))

(defn remove-action
  "An action (key) is moved from the available area to the
  discard pile of game-state (state). Returns the state, with the
  key card moved to the discard pile."
  [state key]
  (let [available (-> state :actions :available)
        index (.indexOf available key) ; Check if key is in the available actions.
        discard (-> state :actions :discard)]

    (if (= index -1) state ; If key is not found, just return the state.
        (-> state
            ; Otherwise, remove the action card at the index.
            (assoc-in [:actions :available] (vec (concat (subvec available 0 index)
                                                         (subvec available (inc index)))))
            ; Add the key to the end of the discard.
            (assoc-in [:actions :discard] (conj discard key))))))

(defn draw-action
  "Take the top card from the action deck of the game state (state), and put it on the bottom
  of the available pile.  Returns the new state."
  [state]
  (let [available (-> state :actions :available) ; Available action cards.
        top-card (-> state :actions :deck first) ; Top card of draw deck.
        rest-of-deck (-> state :actions :deck rest vec)] ; Rest of the draw deck.

    (-> state
        ; The new deck is all but the top card. I.e. the rest of the deck.
        (assoc-in [:actions :deck] rest-of-deck)
        ; The new available pile adds the top card to the available cards.
        (assoc-in [:actions :available] (conj available top-card)))))

(def testdata
  { :color-player {:win :purple, :lose :green},
    :shape-player {:win :bacon, :lose :star},
    :board [[:blue :circle]  [:green :bacon]  [:purple :circle] [:blue :plus]
            [:orange :plus]  [:blue :star]    [:orange :circle] [:purple :star]
            [:green :star]   [:orange :star]  [:blue :bacon]    [:purple :plus]
            [:purple :bacon] [:green :circle] [:green :plus]    [:orange :bacon]],
    :actions {:available [:col-north :ew-reverse :corner-counterclockwise :row-east],
              :deck      [:row-west :col-south :ns-do-si-do :ns-reverse :ew-do-si-do
                          :corner-clockwise]
              :discard []}})

(comment
  (def shuff1
    [[:green :star] [:orange :bacon] [:green :circle] [:orange :circle]
    [:purple :plus] [:orange :star] [:purple :bacon] [:orange :plus]
    [:blue :circle] [:purple :star] [:blue :bacon] [:blue :plus]
    [:green :bacon] [:green :plus] [:purple :circle] [:blue :star]])
  (def shuff2
    [[:purple :plus] [:green :bacon] [:purple :star] [:orange :circle]
     [:green :circle] [:purple :plus] [:orange :bacon] [:purple :star]
     [:purple :star] [:orange :circle] [:purple :plus] [:green :bacon]
     [:orange :bacon] [:purple :star] [:green :circle] [:purple :plus]]
    )

  (def sample1 [[:purple :plus] [:purple :circle] [:purple :star] [:orange :star]])

  (def samplefail [[:purple :plus] [:blue :star] [:orange :bacon] [:blue :bacon]])

  (def perm "All the permutations of tiles." (combo/permutations tiles))

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

(defn card-tests? [s]
    (when ((some-fn any-rc-match? test-push-north? test-push-east? test-push-south? test-push-west?) s) true))

  )
