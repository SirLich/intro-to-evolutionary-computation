(ns intro-to-ec.search-with-heuristic
  (:require [clojure.set :as cset]
            [shams.priority-queue :as pq]))


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

(defn astar-search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [came-from {start-node :start-node}
         cost-so-far {start-node 0}
         frontier (pq/priority-queue #(+ (heuristic %) (cost-so-far %)) :elements [start-node] :variant :set)
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
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
          (recur
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (reduce (fn [cf child] (assoc cf child current-node)) #(inc (get cost-so-far current-node)) kids)
           (add-children
            (pop frontier)
            kids)
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
