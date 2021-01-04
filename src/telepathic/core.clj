(ns telepathic.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))
(comment
  ;;  TODO Set of action cards.
  ;;  TODO Set up COLOR player and SHAPE player. Give each player a goal card and a lose condition card.
  ;;  TODO Function tests whether game is won based on board state & player conditions.
  ;;  TODO Function tests whether game is lost based on board state & player conditions.
  ;;  TODO Game sequence:
  ;;  TODO 1. Start with color player.
  ;;  TODO 2. Current player selects action card.
  ;;  TODO 3. Shape player applies action card to the the game state.
  ;;  TODO 4. Check to see if the game is lost, and conclude the game if it is.
  ;;  TODO 5. Either player may make an Announcemnt.
  ;;  TODO 6. If no announcement is made, the other player begins their turn.
  ;;
  )
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def colors [:purple :blue :green :orange])

(def shapes [:plus :circle :star :bacon])

(def tiles (vec (apply concat (into [] (for [color colors] (into [] (for [shape shapes] [color shape]))))))) ; added (apply concat ...) to flatten one level. -- sws

(defn match3
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
    (match3 (first (apply map vector (first (partition 3 1 set)))))
    (match3 (first (apply map vector (second (partition 3 1 set)))))
    (match3 (second (apply map vector (first (partition 3 1 set)))))
    (match3 (second (apply map vector (second (partition 3 1 set)))))))

(def perm "All the permutations of tiles." (combo/permutations tiles))

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
  "Do any row or column have a matching set of 3?"
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
  [[%1 %2 %3 %4]]
  (seq [%2 %3 %4 %1])

(defn push-one-row-east
  "Takes in a set of 16 tiles, and pushes one 'rownum' to the east."
  [s rownum]
  (vec (apply concat (for [i (range 4)]
                       (if (= i rownum)
                         (push-one-row-forwards (take 4 (drop (* i 4) s)))
                         (take 4 (drop (* i 4) s))))))))

(defn push-one-row-west
  "Takes in a set of 16 tiles, and pushes one 'rownum' to the west."
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

(defn test-push-east?   "Apply 'push west' on each row, and checks for any-rc-match? after each push."
  [s]
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-east s %)) (range 4))))))

(defn test-push-west? "Apply 'push west' on each row, and checks for any-rc-match? after each push." [s]
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-west s %)) (range 4))))))

(defn test-push-south? [s]
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-south s %)) (range 4))))))

(defn test-push-north? [s]
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-north s %)) (range 4))))))

(defn test-each-column   "Returns sequence of matched 3s in the 4 columns" [s]
  (test-each-row (rot-90 s)))

(defn test-rc [s]
  (remove #(nil? %) (concat (test-each-row s) (test-each-column s))))

(def sls                                                  ; shuffled-legal-start
  (loop [set (shuffle tiles) i 0]
    (if (or (not (any-rc-match? set)) (> i 100))
      (if (> i 99)
        i
        set)
      (recur (shuffle tiles) (inc i)))))

(defn anols [times-to-try]                                ; approx number of legal sets
  (loop [n times-to-try legal 0 set (shuffle tiles)]
    (if (> n 0)
      (recur (dec n) (if (any-rc-match? set) legal (inc legal)) (shuffle tiles))
      (float (/ legal times-to-try))
      )))

(defn card-tests? [s]
  (when ((some-fn any-rc-match? test-push-north? test-push-east? test-push-south? test-push-west?) s) true))

(def tougher-sls
  (loop [set (shuffle tiles) i 0]
    (if (or (not (card-tests? set)) (> i 999))
      (if (> i 999)
        i
        set)
      (recur (shuffle tiles) (inc i)))))

(defn capitalize-key   "Feed it a keyname, it returns the name back capitalized. [:blue => 'Blue']."
  [k]
  (str/capitalize (name k)))

(defn asset-name
  "Takes in a key-pair (color & shape). Returns the name of the asset." ; :green :bacon => "Green Bacon.png"
  [c s]
  (str (capitalize-key c) " " (capitalize-key s) ".png"))

(comment
  (check4 sample1)
  (check4 samplefail)
  (test-each-row (nth tperm 100))
  (test-each-row (rot-90 (nth tperm 100)))
  (def tp9 (nth tperm 999999))
  (def tpr (nth tperm (rand-int 9999999)))
  (def tpr9 (rot-90 tp9))
  (test-rc tp9)
  (any-rc-match? tp9)
  (def tprf (nth tpermf (rand-int 999)))
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

  (any-rc-match? shuffled-set)

  (def sls                                                  ; shuffled-legal-start
    (loop [set (shuffle tiles) i 0]
      (if (or (not (any-rc-match? set)) (> i 100))
        (if (> i 99)
          i
          set)
        (recur (shuffle tiles) (inc i)))))
  (defn anols [times-to-try]                                ; approx number of legal sets
    (loop [n times-to-try legal 0 set (shuffle tiles)]
      (if (> n 0)
        (recur (dec n) (if (any-rc-match? set) legal (inc legal)) (shuffle tiles))
        (float (/ legal times-to-try))
        )))

  (def sample1 [[:purple :plus] [:purple :circle] [:purple :star] [:orange :star]])

  (def samplefail [[:purple :plus] [:blue :star] [:orange :bacon] [:blue :bacon]])

  (combo/count-permutations tiles)
  ; => 20922789888000 (21 trillion)
  ; 8.2 trillion possibilities of legal starting plays




  )
