(ns intro-to-ec.search-with-heuristic
  (:require [clojure.set :as cset]
            [shams.priority-queue :as pq]
            [clojure.data.priority-map :as pm]))


(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(def astar-search
  {:get-next-node first
   :add-children into})

(def heuristic-search
  {:get-next-node first
   :add-children into})

(def depth-first-search
  {:get-next-node first
   :add-children concat})

(def breadth-first-search
  {:get-next-node first
   :add-children #(concat %2 %1)})

(def random-search
  {:get-next-node rand-nth
   :add-children concat})

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

;(swh/assearch swh/astar-search (walls/make-grid-problem walls/min-range walls/max-range walls/no-walls) [5 5] 100)
(defn assearch
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [came-from {start-node :start-node}
         cost-so-far {start-node 0}
         frontier (pm/priority-map start-node 0)
         ;(pq/priority-queue #(+ (heuristic %) (get cost-so-far %)) :elements [start-node] :variant :set)
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (println get-next-node)
    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (filter
                    #(or (not (contains? cost-so-far %))
                          (< (inc (get cost-so-far current-node)) (get cost-so-far %)))
                    (remove-previous-states
                    (make-children current-node) frontier (keys came-from)))]
          (println "In loop: " cost-so-far)
          (println kids)
          (println current-node)
          ;(println (get cost-so-far current-node))
          ;(println (pop frontier))
          ;(println (add-children (pop frontier) kids))
          (println "We got past into")
          (recur
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (reduce
            (fn [cf child] (assoc cf child (inc (get cost-so-far current-node))))
            cost-so-far kids)
            ;( #(+ (heuristic %) (get cost-so-far %)) child)
            (reduce (fn [front child] (assoc front child 0))
            frontier kids)
          ; (add-children
           ;  (pop frontier)
           ;  kids)
           (inc num-calls)))))))

(defn search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (pq/priority-queue heuristic :elements [start-node] :variant :set)
         came-from {start-node :start-node}
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
          (recur
           (add-children
            (pop frontier)
            kids)
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
