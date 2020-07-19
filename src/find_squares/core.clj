(ns find-squares.core)

(def make-board
  (->> (for [row (range 8)
             column (range 8)]
         [row column])
       (reduce (fn [collection coords]
                 (assoc collection coords "⬜️")) {})))

(def board (atom make-board))

(def initialize-board
  (do (swap! board  assoc [3 3] "⚪️")
(swap! board assoc [4 4] "⚪️")
(swap! board  assoc [3 4] "⚫️")
(swap! board assoc [4 3] "⚫️")))

(def change {:diagonal-up-left [-1 -1]
             :up [-1 0]
             :diagonal-up-right [-1 1]
             :left [0 -1]
             :right [0 1]
             :diagonal-down-left [1 -1]
             :down [1 0]
             :diagonal-down-right [1 1]})

(defn get-neighbouring-squares
  [current-square]
  (mapv (fn [[k v]]
          (->> (mapv + current-square v)
               (assoc {} k))) change))

(defn find-adjacent-opponent-squares
  [neighbours]
  (filter (fn [k]
            (= (@board (first (vals k))) "⚪️")) neighbours))

(defn check-next-square
  [square]
  (let [next-square-value (mapv + (change (first (keys square))) (first (vals square)))]
    (cond
      (= (@board next-square-value) "⬜️") {(first (keys square)) next-square-value}
      (= (@board next-square-value) "⚪️") (check-next-square {(first (keys square)) next-square-value})
      :else false)))
(check-next-square {:left [3 3]})

(defn is-empty?
  [squares]
  (->> squares
       (map check-next-square)))

(defn find-valid-moves
  [squares]
  (-> squares
    (get-neighbouring-squares)
    (find-adjacent-opponent-squares)
    (is-empty?)))

(map find-valid-moves [[3 4] [4 3]])

(swap! board assoc [3 2] "⚫️")
