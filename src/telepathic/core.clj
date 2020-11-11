(ns telepathic.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def colors [:purple :blue :green :orange])

(def shapes [:plus :circle :star :bacon])

(def tiles (vec (apply concat (into [] (for [color colors] (into [] (for [shape shapes] [color shape]))))))) ; added (apply concat ...) to flatten one level. -- sws

(def tiles2 (into [] (for [color colors] (into [] (for [shape shapes] [color shape])))))

(defn match3 [set]
  (when (apply = set)
    (first set)))

(defn check4 [set]
  "Take in a set of 4 paired items.
  Check to see if any 3 contiguous items have a matching pattern in any color or shape.
  Returns nil if nothing found, or the matched type."
  (if (< (count set) 4)
    nil
    (or
      (match3 (first (apply map vector (first (partition 3 1 set)))))
      (match3 (first (apply map vector (second (partition 3 1 set)))))
      (match3 (second (apply map vector (first (partition 3 1 set)))))
      (match3 (second (apply map vector (second (partition 3 1 set))))))))

(defn qcheck4 [set]
  "Like check4 function, but quick and stops after the first."
  (or
    (match3 (first (apply map vector (first (partition 3 1 set)))))
    (match3 (first (apply map vector (second (partition 3 1 set)))))
    (match3 (second (apply map vector (first (partition 3 1 set)))))
    (match3 (second (apply map vector (second (partition 3 1 set)))))))

(def perm "All the permutations of tiles." (combo/permutations tiles))

(defn rot-90 [s]
  "Takes a sequence of 16 vector pairs and rotates it 90°."
  (vec (apply concat (apply mapv vector (partition 4 s)))))

(defn any-row-match? [s]
  "Performs qcheck4 function, taking the first row '(take 4 s)', then calling
  itself recursively until all are taken."
  (when (seq s)
    (or (qcheck4 (take 4 s)) (any-row-match? (drop 4 s)))))

(defn any-col-match? [s]
  "The same as any-row-match? function, but performing a 90° rotation first,
  to capture columns."
  (any-row-match? (rot-90 s)))

(defn any-rc-match? [s]
  ((some-fn any-row-match? any-col-match?) s))

(defn test-each-row [s]
  "Returns sequence of matched 3s in the 4 rows"
  (flatten (when (seq s)
             (conj [] (check4 (take 4 s)) (test-each-row (drop 4 s))))))

(defn push-one-row-forwards [[%1 %2 %3 %4]]
  "Inputs a set of 4, outputs set pushed by one."
  (seq [%4 %1 %2 %3]))

(defn push-one-row-backwards [[%1 %2 %3 %4]]
  [%2 %3 %4 %1])

(defn push-one-row-east [s rownum]
  (vec (apply concat (for [i (range 4)]
                       (if (= i rownum)
                         (push-one-row-forwards (take 4 (drop (* i 4) s)))
                         (take 4 (drop (* i 4) s)))))))

(defn push-one-row-west [s rownum]
  (vec (apply concat (for [i (range 4)]
                       (if (= i rownum)
                         (push-one-row-backwards (take 4 (drop (* i 4) s)))
                         (take 4 (drop (* i 4) s)))))))

(defn push-one-row-north [s rownum] (rot-90 (push-one-row-west (rot-90 s) rownum)))

(defn push-one-row-south [s rownum] (rot-90 (push-one-row-east (rot-90 s) rownum)))

(defn test-push-east? [s]
  "Apply 'push east' on each row, and checks for any-rc-match? after each push."
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-east s %)) (range 4))))))

(defn test-push-west? [s]
  "Apply 'push west' on each row, and checks for any-rc-match? after each push."
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-west s %)) (range 4))))))

(defn test-push-south? [s]
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-south s %)) (range 4))))))

(defn test-push-north? [s]
  (not (empty? (filter identity (map #(any-rc-match? (push-one-row-north s %)) (range 4))))))

(defn test-each-column [s]
  "Returns sequence of matched 3s in the 4 columns"
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
      (recur (shuffle tiles) (inc i))))
  )

(defn capitalize-key [k]
  "Feed it a keyname, it returns the name back capitalized. [:blue => 'Blue']."
  (str/capitalize (name k)))

(defn asset-name [c s]
  "Takes in a key-pair (color & shape). Returns the name of the asset." ; :green :bacon => "Green Bacon.png"
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

  ; push-one-row-west on shuffled-set 1
  ; [[:purple :plus] [:green :plus] [:purple :star] [:orange :circle]
  ; [:blue :circle] [:green :circle] [:green :star] [:orange :bacon]
  ; [:blue :plus] [:orange :star] [:blue :star] [:green :bacon]
  ; [:purple :bacon] [:green :star] [:blue :bacon] [:orange :plus]]

  ; push-one-row-north on shuffled-set
  ; [[:orange :bacon] [:green :plus] [:purple :star] [:orange :circle]
  ; [:blue :plus] [:blue :circle] [:green :circle] [:green :star]
  ; [:purple :bacon] [:orange :star] [:blue :star] [:green :bacon]
  ; [:purple :plus] [:green :star] [:blue :bacon] [:orange :plus]]

  ; push-one-row-south on shuffled-set 2
  ;
  ; [[:purple :plus] [:green :plus] [:blue :bacon] [:orange :circle]
  ; [:orange :bacon] [:blue :circle] [:purple :star] [:green :star]
  ; [:blue :plus] [:orange :star] [:green :circle] [:green :bacon]
  ; [:purple :bacon] [:green :star] [:blue :star] [:orange :plus]]

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


  (apply = (first (apply map vector (first (partition 3 1 set)))))
  (apply = (second (apply map vector (first (partition 3 1 set)))))

  (def sample1 [[:purple :plus] [:purple :circle] [:purple :star] [:orange :star]])


  (def samplefail [[:purple :plus] [:blue :star] [:orange :bacon] [:blue :bacon]])

  (combo/count-permutations tiles)
  ; => 20922789888000 (21 trillion)
  ; 8.2 trillion possibilities of legal starting plays

  (defn blah [a s]
    (into {} (map #([a %]) s)))

    (loop [mx (max x y) mn (min x y) n mn]
      (if (= 0 (mod mx n) (mod mn n))
        n
        (recur mx mn (dec n))))

  (defn sometrue [& xs]
    (and (not-every? false? xs) (not-every? true? xs)))

  (defn alf [s]
    (seq s))

  (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
  (defn r [s n]
    (mapcat #(repeat n %1) s))

  (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
  (defn r [n s]
    (apply hash-map (mapcat #(list % n) s)))

  (= 256 (__ [2 5 6] [100 10 1]))                           ; dot product 143
  (defn dp [s1 s2]
    (->> (map vector s1 s2)
         (map (fn [[x y]] (* x y)))
         (apply +)))

  (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))                   ; flatten a seqence #28
  (defn fas [& xs]
    (when-let [s (seq xs)]
      (concat (first s) (fas (rest s)))))
  (= (__ 8) '(1 1 2 3 5 8 13 21))
  (defn fibn [x]
    (cond
          (< x 1) nil
          (= x 1) '(1)
          :else (loop [x x s '(1 1)]
                    (if (= x 2)
                      s
                      (recur (dec x) (apply concat s (+ (last s) (nth s (- (count s) 2)))))))))

  (== (__ 7 5/7 2 3/5) 210)                                 ; Least common multiple #100

  (defn lcm [& s]
    (map #((first s) (second s)) s))

  (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])                ;drop every nth #41
  (defn denth [s x]
     (loop [newseq [] n 0]
       (if  (= n (count s))
         newseq
         (recur
           (if (not= 0 (mod (inc n) x))
               (conj newseq (nth s n))
               newseq)
           (inc n)))))
  (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])                ; 49 Split a sequence.
  (defn sas [x s]
    (into [] (take x s))
    )
  )

