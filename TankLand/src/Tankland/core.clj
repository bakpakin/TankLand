(ns Tankland.core
  [:use [tankgui.tankgui :only [init-graphics do-graphics]]])

(def ^:const size 10)
(def ^:private timescale 250)
(def ^:private board (ref {}))
(def ^:private log-agent (agent []))

(defn- log
  "Logs a message. Can safely be called in a transaction."
  [message]
  (send log-agent #(do (println message)
                     (conj % {:timestamp (System/currentTimeMillis)
                              :message message}))))

(defn- print-full-log
  "Prints the log in a human-readable form."
  []
  (doseq [message @log-agent]
    (println "At" (:timestamp message) (:message message))))

(defn- wrap
  "Given a coordinate pair, wraps the coordinates on the board."
  [[row col]]
  [(mod row size) (mod col size)])

(defn- clear-cell
  "Clears the given cell on the board. Must be called in a transaction."
  [location]
  (alter board dissoc location))

(defn- alive
  "Checks whether a tank is alive.
The value of the tank should be passed, not the ref."
  [tank]
  (> (:health tank) 0))

(defn- tank-death-watch
  "A watch function that will remove a tank from the board
if its health drops to 0 or less."
  [key ref old-state new-state]
  (when (not (alive new-state))
    (dosync
      (clear-cell (:location new-state))
      (log (str (:name new-state) " died.")))))

(defn- add-tank
  "Adds a new tank with the given name. Returns the tank."
  [name]
  (let [tank (ref {:name name :health 100 :energy 1000000})]
    (add-watch tank :death-watch tank-death-watch)
    (dosync (let [locations (for [row (range size), col (range size)
                                  :let [location [row col]]
                                  :when (not (@board location))]
                              location)
                  location (rand-nth locations)]
              (alter tank assoc :location location)
              (alter board assoc location tank)
              (log (str name " was added at " location ".")))
      tank)))

(defn- run
  "Runs Tankland with one tank for each of the given behaviors."
  [& info]
  (init-graphics size size)
  (future (while true (do-graphics @board)))
  (doseq [[name behavior-fn] info]
    (when (and (string? name) behavior-fn)
      (let [tank (add-tank name)]
        (future (while (alive @tank) (behavior-fn tank)))
        (log (str (:name @tank) " started."))))))

(defn- kill-all-tanks
  "KILL. ALL. THE. TANKS."
  []
  (doseq [tank (vals @board) :when (map? @tank)]
    (dosync (alter tank assoc :health 0))))

(defmacro ^:private with-relative-time
  "Causes the body to take an amount of time equal to
(* timescale relative-time) in milliseconds to execute."
  [relative-time & body]
  `(let [end-time# (+ (System/currentTimeMillis) (* timescale ~relative-time))
         val# ~@body]
     (while (> end-time# (System/currentTimeMillis)))
     val#))

(defn- use-energy
  "Deplete a tank's energy by some amount,
or return false if the tank doesn't have enough energy."
  [tank energy]
  (dosync
    (when (>= (:energy @tank) energy)
      (alter tank update-in [:energy] - energy)
      (log (str (:name @tank) " used " energy " energy.")))))

(defmacro ^:private do-tank-action
  "If the tank is alive and has enough energy, uses that much energy,
and executes the body in a dosync with the relative time cost."
  [tank energy relative-time & body]
  `(when (and (alive (deref ~tank)) (use-energy ~tank ~energy))
     (with-relative-time ~relative-time
       (dosync ~@body))))

(def ^:private ^:const directions
  {:N [-1 0] :NE [-1 1] :E [0 1] :SE [1 1] :S [1 0] :SW [1 -1] :W [0 -1] :NW [-1 -1]})

(defn- new-location
  "Obtain a new location from an old location and a direction. Wraps."
  [old-loc direction]
  (wrap (vec (map + old-loc (directions direction)))))

(defn- deref'
  "Derefs a reference, but doesn't try to deref a value."
  [x]
  (if (instance? clojure.lang.IReference x) @x x))

; Begin tank helper functions

(defn rand-direction
  "Selects a random direction."
  []
  (rand-nth (keys directions)))

(defn move
  "Moves the tank in the specified direction if unobstructed.
Costs 100000 energy and takes 1 time unit, even if there is an obstruction."
  [tank direction]
  (do-tank-action
    tank 0 1
    (let [old-loc (:location @tank)
          new-loc (new-location old-loc direction)
          occupant (@board new-loc)]
      (when (number? (deref' occupant))
        (alter tank update-in [:health] - occupant)
        (clear-cell new-loc))
      (when (nil? occupant)
        (alter tank assoc :location new-loc)
        (alter board assoc new-loc tank)
        (clear-cell old-loc)
        (log (str (:name @tank) " moved from " old-loc " to " new-loc ".")))
      (when occupant
        (log (str (:name @tank) " was blocked when trying to move from "
                  old-loc " to " new-loc "."))))))
