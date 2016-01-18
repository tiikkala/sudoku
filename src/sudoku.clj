(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (loop [row (first coord)
         col 0
        a-set #{}]
    (if (= col (count board))
      a-set
      (recur row (inc col) (conj a-set (value-at board [row col]))))))

(defn col-values [board coord]
  (loop [col (second coord)
         row 0
         a-set #{}]
    (if (= row (count board))
      a-set
      (recur col (inc row) (conj a-set (value-at board [row col]))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left [coords]
  (let [[row column] coords]
    (cond (and (< row 3) (< column 3)) [0 0]
          (and (< row 6) (< column 3)) [3 0]
          (and (< row 9) (< column 3)) [6 0]
          (and (< row 3) (> column 5)) [0 6]
          (and (< row 6) (> column 5)) [3 6]
          (and (< row 9) (> column 5)) [6 6]
          (and (< row 3) (> column 2)) [0 3]
          (and (< row 6) (> column 2)) [3 3]
          (and (< row 9) (> column 2)) [6 3]
          )))

(defn block-values [board coord]
  (loop [[row col] (top-left coord)
         a-set #{}
         i 0]
    (cond
     (>= i 3) a-set
     (< col 3) (recur [(inc row) col]
                      (apply conj a-set (subvec (get board row) 0 3))
                      (inc i))
     (< col 5) (recur [(inc row) col]
                      (apply conj a-set (subvec (get board row) 3 6))
                      (inc i))
     (< col 9) (recur [(inc row) col]
                      (apply conj a-set (subvec (get board row) 6))
                      (inc i)))))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0)
    #{}
    (let [block (set/difference all-values (block-values board coord))
          col-row (set/union (col-values board coord) (row-values board coord))]
      (set/difference block col-row))))

(defn filled? [board]
  (loop [row 0
         a-set #{}]
    (cond
     (contains? a-set 0) false
     (= row 9) true
     :else (recur (inc row) (apply conj a-set (row-values board [row 0]))))))

(defn rows [board]
  (loop [row 0
         v []]
    (if (= row 9)
      v
      (recur (inc row) (conj v (row-values board [row 0]))))))

(defn valid-rows? [board]
  (loop [row 0]
    (cond
     (> row 8) true
     (not (empty? (set/difference all-values (row-values board [row 0])))) false
      :else (recur (inc row)))))

(defn cols [board]
  (loop [col 0
         v []]
    (if (= col 9)
      v
      (recur (inc col) (conj v (col-values board [0 col]))))))

(defn valid-cols? [board]
  (loop [col 0]
    (cond
     (> col 8) true
     (not (empty? (set/difference all-values (col-values board [0 col])))) false
     :else (recur (inc col)))))

(defn blocks [board]
  (loop [row 0
         col 0
         v []]
    (cond
     (> col 6) v
     (= row 6) (recur 0 (+ col 3) (conj v (block-values board [row col])))
     :else (recur (+ row 3) col (conj v (block-values board [row col]))))))

(defn valid-blocks? [board]
  (loop [values (blocks board)]
    (cond
     (empty? values) true
     (not (empty? (set/difference all-values (first values)))) false
     :else (recur (rest values)))))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0]
    (cond
     (> row 8) nil
     (contains? (row-values board [row 0]) 0) [row (.indexOf (get board row) 0)]
     :else (recur (inc row)))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board) [current-board] [])
    (let [empty-position (find-empty-point current-board)]
      (for [valid-value-for-point (valid-values-for current-board empty-position)
            solution (solve-helper (set-value-at current-board empty-position valid-value-for-point))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
