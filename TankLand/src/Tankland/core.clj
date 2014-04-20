(ns Tankland.core
  [:refer-clojure :exclude [name]]
  [:require [clojure.java.io :as io]]
  [:use tankgui.dantankgui])

(def ^:const size 10)
(def ^:const wrap false)
(def ^:const starting-energy 1000)
(def ^:const max-energy (* 2 starting-energy))
(def ^:const energy-constants
  {:move 0 :place-mine 0 :defuse-mine 0 :scan-line 0 :scan-area 0
   :fire-artillery 0 :fire-bullet 0 :repair 0 :recharge -10
   :activate-shield 0})
(def ^:const time-costs
  {:move 1 :place-mine 1 :defuse-mine 1 :scan-line 1 :scan-area 1
   :fire-artillery 1 :fire-bullet 1 :repair 1 :recharge "N/A"
   :activate-shield 1})
(def ^:private timescale 250)
(def ^:private recharge-rate 1)

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
  "Logs the string concatenation of message.
Can safely be called in a transaction."
  [& message]
  (let [message (apply str message)]
    (send log-agent #(do (log-message message)
                       (conj % {:timestamp (System/currentTimeMillis)
                                :message message
                                :game-state (deref-walk {:board board
                                                         :tanks tanks})})))))

(defn- print-full-log
  "Prints the log in a human-readable form."
  []
  (doseq [message @log-agent]
    (println "At" (:timestamp message) (:message message))))

(defn- show-message
  "Pops up a message in the GUI."
  [message]
  (javax.swing.JOptionPane/showMessageDialog (graphics-frame) message))

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
  (when (and (not (alive new-state)) (alive old-state))
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
    (let [tank (ref {:name name :health 100 :energy starting-energy
                     :shield 0 :information {}})
          tank-placed? (ref false)]
      (dosync (if-let [locations (seq (for [row (range size), col (range size)
                                            :let [location [row col]]
                                            :when (not (get-cell location))]
                                        location))]
                (let [location (rand-nth locations)]
                  (alter tank assoc :location location)
                  (alter board assoc location tank)
                  (alter tanks assoc name tank)
                  (log name " was added at " location ".")
                  (alter tank-placed? (constantly true)))))
      (when @tank-placed?
        (add-watch tank :death-watch tank-death-watch)
        (future (while (alive @tank) (Thread/sleep timescale)
                  (dosync (alter tank update-in [:energy]
                                 #(min (+ % recharge-rate) max-energy)))))
        tank))))

(defn- run
  "Runs Tankland with one tank for each of the given behaviors.
Assumes that all arguments are legal tanks."
  [& info]
  (init-graphics size)
  (future (while true
            (do-graphics @board)
              (Thread/sleep 33)))
  (future (while true
            (do-tanks @tanks)
              (Thread/sleep timescale)))
  (doseq [[name behavior-fn] info]
    (if-let [tank (add-tank name)]
      (future (try (while (alive @tank)
                     (behavior-fn tank))
                (catch Exception e
                  (dosync (alter tank assoc :health 0)
                    (log "The creators of " name " lose one year point.")))))
      (log name " started.")))
  (add-watch tanks :victory
             #(when (and (= (count %4) 1) (> (count %3) 1))
                (let [message (str  (first (keys %4)) " wins!")]
                  (log message)
                  (future (show-message message)))))
  nil)

(defn- legal-tank?
  "Checks whether a tank is legal."
  [[name behavior-fn]]
  (and (string? name)
       (not-any? #{`deref 'deref 'dosync}
                 (flatten behavior-fn))
       (try (let [temp (gensym "ns")]
              (with-bindings {#'*ns* (create-ns temp)}
                (refer-clojure :exclude ['name])
                (refer 'Tankland.core)
                (eval behavior-fn))
              (remove-ns temp)
              true)
         (catch Exception e false))))

(defn- run-from-files
  "Runs Tankland with tanks read in from specified files. If no files are
specified, uses every .tnk file in the present working directory."
  ([& file-names]
  (try
    (let [tanks (with-bindings {#'*read-eval* false}
                  (read-string (str \( (apply str (map slurp file-names)) \))))
          legal-tanks (filter legal-tank? tanks)
          illegal-count (- (count tanks) (count legal-tanks))]
      (when (pos? illegal-count)
        (show-message (str illegal-count " illegal tanks.")))
      (apply run (map eval tanks)))
    (catch Exception e (println "Error reading tanks from file."))))
  ([] (apply run-from-files
             (filter #(.endsWith % ".tnk")
                     (map #(.getName %)
                          (file-seq (io/file ".")))))))

(defn- kill-all-tanks
  "KILL. ALL. THE. TANKS."
  []
  (dosync (doseq [tank (vals @tanks)] (alter tank assoc :health 0))))

(defmacro ^:private with-relative-time
  "Causes the body to take an amount of time equal to
(* timescale relative-time) in milliseconds to execute."
  [relative-time & body]
  `(let [end-time# (+ (System/currentTimeMillis) (* timescale ~relative-time))
         val# ~@body]
     (while (> end-time# (System/currentTimeMillis)))
     val#))

(declare name, energy, shield)

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
  `(defn ~attr
     ~(str "Gets the " attr " of the tank. Costs no energy.")
     [~'tank]
     (~(keyword (str attr)) (deref ~'tank))))

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

(defn- deal-damage
  "Deals damage to a tank, reduced if the tank has an active shield, and logs it."
  [tank damage]
  (dosync
    (let [damage (* damage (- 1 (shield tank)))]
      (alter tank update-in [:health] - damage)
      (log (name tank) " took " damage " damage."))))

; Begin tank helper functions

(defaccessor health)
(defaccessor energy)
(defaccessor name)
(defaccessor location)
(defaccessor shield)

(defn rand-direction
  "Selects a random direction."
  []
  (rand-nth (keys directions)))

(defn move
  "Moves the tank in the specified direction if unobstructed.
Costs 1000 energy and takes 1 time unit, even if there is an obstruction.
Energy cost is constant."
  [tank direction]
  (do-tank-action
    tank (energy-constants :move) (time-costs :move)
    (let [old-loc (location tank)
          new-loc (new-location old-loc direction)
          occupant #(deref' (get-cell new-loc))]
      (when (number? (occupant))
        (log (name tank) " hit a mine while moving to " new-loc ".")
        (deal-damage tank (occupant))
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
If there is a tank there, the mine will detonate instantly.
Energy cost scales with damage."
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
        (deal-damage occupant damage)))))

(defn defuse-mine
  "If there is a mine in the adjacent square in the given direction, defuse it.
Uses energy even if there is not a mine. Energy cost is constant."
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
each of which will be either :tank, :mine, or :wall.
Energy cost scales with the distance."
  [tank direction distance]
  (do-tank-action
    tank (* (energy-constants :scan-line) distance) (time-costs :scan-line)
    (->> (iterate #(new-location % direction) (location tank))
      rest (take distance)
      scan-cells)))

(defn scan-area
  "Scan an area of the given radius around the tank.
Returns a map of the occupied scanned squares to their occupants,
each of which will be either :tank, :mine, or :wall.
Energy cost scales with the square of the radius."
  [tank radius]
  (do-tank-action
    tank (#(* % % %2) (energy-constants :scan-area) radius) (time-costs :scan-area)
    (scan-cells (area (location tank) radius))))

(defn fire-artillery
  "Fire artillery artillery at a specific location. Does 10 damage if it hits a
tank. Energy cost scales with the distance of the target."
  [tank target]
  (do-tank-action
    tank (* (energy-constants :fire-artillery) (distance (location tank) target))
    (time-costs :fire-artillery)
    (let [occupant (get-cell target)
          log (partial log (name tank) " fired artillery at " target " and ")]
      (if (map? (deref' occupant))
        (do (log " hit " (name occupant) ".")
          (deal-damage occupant 10)
          (log "missed."))))))

(defn fire-bullet
  "Fire a bullet. The bullet will travel in a straight line until it hits a
tank or a wall, and damage decreases with distance, starting at double the size
of the map and decreasing by 2 every square (i.e. on a size 10 board,
shooting an immidiately adjecent tank will do 18 damage).
Energy cost is constant."
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
              (deal-damage occupant damage)
              (log "hit " (name occupant) "."))
            :default
            (recur (- damage 2) (new-location bullet-loc direction))))))))

(defn repair
  "Repairs some damage to a tank. The health of the tank cannot exceed 100,
but energy will still be consumed for unused repairs. It is recommended to use
the health function to obtain the current health of your tank.
Energy cost scales with the amount of damage repaired."
  [tank damage]
  (do-tank-action
    tank (* (energy-constants :repair) damage) (time-costs :repair)
    (alter tank update-in [:health] #(min 100 (+ damage %)))
    (log (name tank) " repaired " damage " damage.")))

(defn recharge
  "Go dormant for some number of time units in order to regain energy.
Energy gained scales with time spent dormant."
  [tank time-units]
  (do-tank-action
    tank (* (energy-constants :recharge) time-units) time-units
    (log (name tank) " recharged for " time-units " time units.")))

(defn activate-shield
  "Activate a shield that blocks some portion of damage (between 0 and 1)
for some amount of time units. Energy cost scales with the number of time units
divided by 1 minus the portion of damage blocked."
  [tank time-units portion]
  (let [portion (rationalize portion)]
    (when (< 0 portion 1)
      (when (do-tank-action
              tank (* (energy-constants :activate-shield) time-units (/ (- 1 portion)))
              (time-costs :activate-shield)
              (alter tank update-in [:shield] #(- 1 (* (- 1 %) portion))))
        (do-graphics @board)
        (future (Thread/sleep (* time-units timescale))
          (dosync (alter tank update-in [:shield] #(+ (/ (- % 1) portion) 1)))
          (do-graphics @board))))))

(defn store-information
  "Stores information in the tank's memory at the given key.
Has no time or energy cost. The information can be retrieved with get-information."
  [tank key information]
  (dosync (alter tank assoc-in [:information key] information)))

(defn delete-information
  "Deletes the information stored in the tanks memory at the given key."
  [tank key]
  (dosync (alter tank #(assoc % :information (dissoc (:information %) key)))))

(defn get-information
  "Gets the information stored in the tank's memory. Has no time or energy cost."
  [tank]
  (:information @tank))