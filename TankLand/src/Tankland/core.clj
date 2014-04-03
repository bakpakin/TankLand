(ns Tankland.core
  [:use [tankgui.tankgui :only [init-graphics do-graphics]]])

(def ^:const size 10)
(def ^:private timescale 1000)
(def ^:private board (ref {}))
(def ^:private log-agent (agent []))

(defn- log
  "Logs a message."
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

(defn- get-cell
  "Returns the occupant of the specified cell on the board:
can be a tank, a mine, or nil. Wraps the cell."
  [location]
  (@board (wrap location)))

(defn- set-cell
  "Sets the occupant of the specified cell on the board:
can be a tank, a mine, or nil. Wraps the cell. Must be called in a transaction."
  [location occupant]
  (alter board assoc (wrap location) occupant))

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
    (log (str (:name new-state) " died."))
    (clear-cell (:location new-state))))

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
              (set-cell location tank)
              (log (str name " was added at " location ".")))
      tank)))

(defn- run
  "Runs Tankland with one tank for each of the given behaviors."
  [& info]
  (init-graphics size size)
  (future (while true (do-graphics board)))
  (doseq [[name behavior-fn] info]
    (when (and (string? name) behavior-fn)
      (let [tank (add-tank name)]
        (future (while (alive @tank) (behavior-fn tank)))
        (log (str (:name @tank) " started."))))))

(defmacro ^:private with-relative-time
  "Causes the body to take an amount of time equal to
(* timescale relative-time) in milliseconds to execute."
  [relative-time & body]
  `(let [end-time# (+ (System/currentTimeMillis) (* timescale ~relative-time))
         val# ~@body]
     (while (> end-time# (System/currentTimeMillis)))
     val#))

(def ^:private ^:const directions
  {:N [-1 0] :NE [-1 1] :E [0 1] :SE [1 1] :S [1 0] :SW [1 -1] :W [0 -1] :NW [-1 -1]})

; Begin tank helper functions

(defn rand-direction
  "Selects a random direction."
  []
  (rand-nth (keys directions)))

(defn move
  "Moves the tank in the specified direction."
  [direction])