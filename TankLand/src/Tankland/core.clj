(ns Tankland.core
  [:use [tankgui.tankgui :only [init-graphics do-graphics]]])

(def ^:const size 10)
(def ^:const wrap false)
(def ^:private timescale 250)
(def ^:private board (ref {}))
(def ^:private tanks (ref (sorted-map)))
(def ^:private log-agent (agent []))

(defn- deref-walk [x]
  "Derefs every reference in a nested data structure."
  (clojure.walk/prewalk 
    (fn [e] 
      (if (instance? clojure.lang.IReference e) 
        (deref-walk (deref e)) 
        e)) 
    x))

(defn- log
  "Logs a the string concatenation of message.
Can safely be called in a transaction."
  [& message]
  (let [message (apply str message)]
    (send log-agent #(do (println message)
            (conj % {:timestamp (System/currentTimeMillis)
                     :message message
                     :game-state (deref-walk {:board board :tanks tanks})})))))

(defn- print-full-log
  "Prints the log in a human-readable form."
  []
  (doseq [message @log-agent]
    (println "At" (:timestamp message) (:message message))))

(defn- get-cell
  "Gets the occupant of a cell on the board. Returns nil if cell is empty.
Returns :wall if cell is off the board."
  [[row col :as cell]]
  (if (and (< -1 row size) (< -1 col size))
    (@board cell)
    :wall))

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
      (alter tanks dissoc (:name new-state))
      (log (:name new-state) " died."))))

(defn- add-tank
  "Adds a new tank with the given name. Returns the tank. If there is no room
on the board, or there is another tank of the same name, returns nil."
  [name]
  (if (contains? @tanks name)
    (log "A tank already has the name " name ".")
    (let [tank (ref {:name name :health 100 :energy (* size size 10)})]
      (add-watch tank :death-watch tank-death-watch)
      (dosync (if-let [locations (seq (for [row (range size), col (range size)
                                            :let [location [row col]]
                                            :when (not (get-cell location))]
                                        location))]
                (let [location (rand-nth locations)]
                  (alter tank assoc :location location)
                  (alter board assoc location tank)
                  (alter tanks assoc name tank)
                  (log name " was added at " location ".")
                  tank))))))

(declare name, energy)

(defn- run
  "Runs Tankland with one tank for each of the given behaviors."
  [& info]
  (init-graphics size)
  (do-graphics @board) ; just in case there are already things on the board
  (add-watch board :graphics #(do-graphics %4))
  (doseq [[name behavior-fn] info]
    (when (and (string? name) behavior-fn)
      (if-let [tank (add-tank name)]
        (future (while (alive @tank) (behavior-fn tank)))
        (log name " started.")))))

(defn- kill-all-tanks
  "KILL. ALL. THE. TANKS."
  []
  (doseq [tank (vals @tanks)]
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
  [tank energy-use]
  (dosync
    (when (>= (energy tank) energy-use)
      (alter tank update-in [:energy] - energy-use)
      (log (name tank) " used " energy-use " energy."))))

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
  (let [[row col] (vec (map + old-loc (directions direction)))]
    (if wrap [(mod row size) (mod col size)] [row col])))

(defn- deref'
  "Derefs a reference, but doesn't try to deref a value."
  [x]
  (if (instance? clojure.lang.IReference x) @x x))

(defmacro ^:private defaccessor
  "Defines an accessor function for the tank attribute of the same name.
The resulting function will be public."
  [attr]
  `(defn ~(symbol attr)
     ~(str "Gets the " attr " of the tank.")
     [~'tank]
     (~(keyword attr) (deref ~'tank))))

(defn- occupant-type
  "Returns the type of a cell occupant, nil, :tank, :mine, :wall, or :other."
  [cell]
  (let [occupant (deref' (get-cell cell))]
    (cond
      (map? occupant) :tank
      (number? occupant) :mine
      (nil? occupant) nil
      (= :wall occupant) :wall
      :default :other)))

(defn- scan-cells
  "Takes a list of cells to scan and returns a map with the occupied cells."
  [cells]
  (->> cells
    (map (fn [cell] {cell (occupant-type cell)}))
    (filter #(first (vals %)))
    (into {})))

(defn- area
  "Generates a list of the cells in an area, excluding the center."
  [[row col] radius]
  (for [dr (range (- radius) (inc radius))
        dc (range (- radius) (inc radius))
        :when (not= 0 dr dc)]
    (if wrap [(mod (+ row dr) size) (mod (+ col dc) size)]
      [(+ row dr) (+ col dc)])))

(defn- distance
  "Calculates the distance between two locations."
  [[r1 c1] [r2 c2]]
  (min (Math/abs (- r1 r2)) (Math/abs (- c1 c2))))

(def ^:const energy-constants
  {:move 0 :place-mine 0 :defuse-mine 0 :scan-line 0 :scan-area 0
   :fire-artillery 0 :fire-bullet 0 :repair 0 :recharge -10})

(def ^:const time-costs
  {:move 1 :place-mine 1 :defuse-mine 1 :scan-line 1 :scan-area 1
   :fire-artillery 1 :fire-bullet 1 :repair 1 :recharge "N/A"})

; Begin tank helper functions

(defaccessor "health")
(defaccessor "energy")
(defaccessor "name")
(defaccessor "location")

(defn rand-direction
  "Selects a random direction."
  []
  (rand-nth (keys directions)))

(defn move
  "Moves the tank in the specified direction if unobstructed.
Costs 1000 energy and takes 1 time unit, even if there is an obstruction."
  [tank direction]
  (do-tank-action
    tank (energy-constants :move) (time-costs :move)
    (let [old-loc (location tank)
          new-loc (new-location old-loc direction)
          occupant #(deref' (get-cell new-loc))]
      (when (number? (occupant))
        (alter tank update-in [:health] - (occupant))
        (log (name tank) " took " (occupant) " damage from a mine.")
        (clear-cell new-loc))
      (if (nil? (occupant))
        (do (alter tank assoc :location new-loc)
          (alter board assoc new-loc tank)
          (clear-cell old-loc)
          (log (name tank) " moved from " old-loc " to " new-loc "."))
        (log (name tank) " was blocked when trying to move from "
                  old-loc " to " new-loc ".")))))

(defn place-mine
  "Place a mine that does the given amount of damage
in the adjecent space in the given direction.
If there is already a mine there, the mines will combine.
If there is a tank there, the mine will detonate instantly."
  [tank damage direction]
  (do-tank-action
    tank (* (energy-constants :place-mine) damage) (time-costs :place-mine)
    (let [mine-loc (new-location (location tank) direction)
          occupant (get-cell mine-loc)]
      (log (name tank) " placed a " damage "-damage mine at " mine-loc ".")
      (cond
        (nil? occupant) (alter board assoc mine-loc (ref damage))
        (number? (deref' occupant)) (alter occupant + damage)
        (map? (deref' occupant))
        (do (alter occupant update-in [:health] - damage)
          (log "The mine " (name tank) " placed under " (name occupant)
               " detonated and dealt " damage " damage."))))))

(defn defuse-mine
  "If there is a mine in the given direction, defuse it.
Uses energy even if there is not a mine."
  [tank direction]
  (do-tank-action
    tank (energy-constants :defuse-mine) (time-costs :defuse-mine)
    (let [defuse-loc (new-location (location tank) direction)]
      (when (number? (deref' (get-cell defuse-loc)))
        (clear-cell defuse-loc)
        (log (name tank) " defused the mine at " defuse-loc ".")))))

(defn scan-line
  "Scan in a line in one direction for some distance.
Returns a map of the occupied scanned squares to their occupants,
each of which will be either :tank, :mine, or :wall."
  [tank direction distance]
  (do-tank-action
    tank (* (energy-constants :scan-line) distance) (time-costs :scan-line)
    (->> (iterate #(new-location % direction) (location tank))
     rest (take distance)
     scan-cells)))

(defn scan-area
  "Scan an area of the given radius around the tank.
Returns a map of the occupied scanned squares to their occupants,
each of which will be either :tank, :mine, or :wall."
  [tank radius]
  (do-tank-action
    tank (#(* % % %2) (energy-constants :scan-area) radius) (time-costs :scan-area)
    (scan-cells (area (location tank) radius))))

(defn fire-artillery
  "Fire artillery artillery at a specific location.
Does 10 damage if it hits a tank."
  [tank target]
  (do-tank-action
    tank (* (energy-constants :fire-artillery) (distance (location tank) target))
    (time-costs :fire-artillery)
    (let [occupant (get-cell target)
          log (partial log (name tank) " fired artillery at " target " and ")]
      (if (map? (deref' occupant))
        (do (alter occupant update-in [:health] - 10)
          (log " hit " (name occupant) "."))
        (log "missed.")))))

(defn fire-bullet
  "Fire a bullet. The bullet will travel in a straight line until it hits a
tank or a wall, and damage decreases with distance, starting at double the size
of the map and decreasing by 2 every square (i.e. on a size 10 board,
shooting an immidiately adjecent tank will do 18 damage)."
  [tank direction]
  (do-tank-action
    tank (energy-constants :fire-bullet) (time-costs :fire-bullet)
    (let [log (partial log (name tank) " shot "
                       (clojure.core/name direction) " and ")] 
      (loop [damage (- (* 2 size) 2)
             bullet-loc (new-location (location tank) direction)]
        (let [occupant (get-cell bullet-loc)]
          (cond
            (= :wall occupant)
            (log "hit the wall at " bullet-loc ".")
            (map? (deref' occupant))
            (do
              (alter occupant update-in [:health] - damage)
              (log "dealt " damage " damage to " (name occupant) "."))
            :default
            (recur (- damage 2) (new-location bullet-loc direction))))))))

(defn repair
  "Repairs some damage to a tank. The health of the tank cannot exceed 100,
but energy will still be consumed for unused repairs. It is recommended to use
the health function to obtain the current health of your tank."
  [tank damage]
  (do-tank-action
    tank (* (energy-constants :repair) damage) (time-costs :repair)
    (alter tank update-in [:health] #(min 100 (+ damage %)))
    (log (name tank) " repaired " damage " damage.")))

(defn recharge
  "Go dormant for some number of time units in order to regain energy.
The more time spent dormant, the more energy gained."
  [tank time-units]
  (do-tank-action
    tank (* (energy-constants :recharge) time-units) time-units
    (log (name tank) " recharged for " time-units " time units.")))
