(ns tankgui.tankgui
  (:import javax.swing.JPanel)
  (:import javax.swing.JFrame)
  (:import java.awt.Dimension)
  (:import java.awt.Color)
  (:import java.awt.event.WindowAdapter)
  (:import java.awt.event.MouseAdapter))

(defn- load-image
  [classpath]
  (javax.imageio.ImageIO/read (java.lang.ClassLoader/getSystemResourceAsStream classpath)))

;to add images, put them in the classpath and load them as done below
(def images
  {:tank (load-image "tankgui/tank.png")
   :mine (load-image "tankgui/mine.png")
   :? (load-image "tankgui/qmark.png")
   })

(defn- draw-cells
  [g board cell-size]
  (.setColor g Color/WHITE)
  (doseq [[[y x] val] board]
           (.drawImage g 
             (cond
               (not (instance? clojure.lang.IReference val)) (images :?)
               (number? @val) (images :mine)
               (map? @val) (images :tank)
               true (images :?);default case
               )
             (inc (int (* x cell-size)))
             (inc (int (* y cell-size)))
             (dec cell-size)
             (dec cell-size)
             nil)))

(defn- draw-grid
  [g width height cell-size]
  (let [x1 0
        y1 0
        x2 (* cell-size width)
        y2 (* cell-size height)]
    (doseq [x (range x1 (inc x2) cell-size)]
      (.drawLine g x y1 x y2))
    (doseq [y (range y1 (inc y2) cell-size)]
      (.drawLine g x1 y x2 y))))

(defn- draw-grid-infinite
  [g cell-size]
  (let [r (.getClipBounds g)
        x1 (int (* (Math/floor (double (/ (.getX r) cell-size))) cell-size))
        y1 (int (* (Math/floor (double (/ (.getY r) cell-size))) cell-size))
        x2 (+ x1 (.getWidth r) cell-size)
        y2 (+ y1 (.getHeight r) cell-size)]
    (doseq [x (range x1 x2 cell-size)]
      (.drawLine g x y1 x y2))
    (doseq [y (range y1 y2 cell-size)]
      (.drawLine g x1 y x2 y))))

(defn- make-panel
  [viewer]
  (proxy [JPanel] [] (paint [g] 
                       (.translate g @(viewer :x) @(viewer :y)) 
                       (if (or (= -1 @(viewer :width)) (= -1 @(viewer :height)))
                         (draw-grid-infinite g @(viewer :cell-size))
                         (draw-grid g @(viewer :width) @(viewer :height) @(viewer :cell-size))
                       )
                       (draw-cells g @(viewer :board) @(viewer :cell-size)))))

(defn- make-panel-with-frame
  [viewer]
  (let [panel (make-panel viewer)
        cs @(viewer :cell-size)
        w @(viewer :width)
        h @(viewer :height)]
    (if (or (= -1 w) (= -1 h))
          (.setPreferredSize panel (new Dimension 800 600))
          (.setPreferredSize panel (new Dimension (* cs w) (* cs h)))
    )
    (doto (new JFrame) 
      (.setContentPane panel) 
      .pack 
      (.setVisible true))
    panel))

(defn add-drag-control
  "Adds the ability to drag the camera in the viewer around."
  [viewer]
  (.addMouseMotionListener @(viewer :display-panel)
    (proxy [MouseAdapter] [] 
      (mouseDragged [e]
        (swap! (viewer :x) + (- (.getX e) @(viewer :mousex)))
        (swap! (viewer :y) + (- (.getY e) @(viewer :mousey)))
        (reset! (viewer :mousex) (.getX e)) 
        (reset! (viewer :mousey) (.getY e))
        (.repaint @(viewer :display-panel)))
      (mouseMoved [e] 
        (reset! (viewer :mousex) (.getX e)) 
        (reset! (viewer :mousey) (.getY e)))))
  viewer)

(defn make-viewer
  "Makes a new viewer that displays a board of given width and height, 
   or infinte width and height if no dimensions are supplied."
  ([cell-size width height]
  (let [viewer 
        {
        :board (atom {})
        :display-panel nil
        :cell-size (atom cell-size)
        :x (atom 0)
        :y (atom 0)
        :width (atom width)
        :height (atom height)
        :mousex (atom 0)
        :mousey (atom 0)
        }]
  (assoc viewer
          :display-panel (atom (make-panel-with-frame viewer)))))
  ([cell-size]
  (make-viewer cell-size -1 -1)))

(defn update-viewer
  "Updates the board of the viewer. Also calls a repaint."
  [viewer board]
  (reset! (viewer :board) board)
  (.repaint @(viewer :display-panel)))


(defn init-graphics
  "Initilaizes the gui with a board of specified height and width."
  ([width height]
  (def ^:private viewer (make-viewer 64 width height)))
  ([size] (init-graphics size size))
  ([] (init-graphics -1 -1)))

(defn do-graphics
  "Updates the gui to show the current board."
  [board]
  (update-viewer viewer board))