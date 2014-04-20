(ns tankgui.tankland
  (:import javax.swing.JPanel)
  (:import javax.swing.JFrame)
  (:import java.awt.Dimension)
  (:import java.awt.Color)
  (:import java.awt.event.WindowAdapter)
  (:import java.awt.event.MouseAdapter)
  (:import java.awt.event.ComponentAdapter)
  (:import javax.swing.JSplitPane)
  (:import javax.swing.JList)
  (:import javax.swing.JScrollPane)
  (:import javax.swing.DefaultListModel)
  (:import javax.swing.ListCellRenderer)
  (:import javax.swing.JLabel)
  (:import javax.swing.BoxLayout)
  (:import javax.swing.border.BevelBorder)
  (:import javax.swing.JViewport)
  (:import javax.swing.JEditorPane)
  (:import java.io.PrintWriter)
  (:import javax.swing.text.PlainDocument)
  (:import tankgraphics.boardFrame)
  (:import java.awt.event.ActionListener)
  (:import javax.swing.event.ChangeListener))

(defn- load-image
  [classpath]
  (javax.imageio.ImageIO/read (java.lang.ClassLoader/getSystemResourceAsStream classpath)))

;to add images, put them in the classpath and load them as done below
(def images
  {:tank (load-image "tankgui/tank.png")
   :mine (load-image "tankgui/mine.png")
   :other (load-image "tankgui/qmark.png")
   :wall (load-image "tankgui/wall.png")
   :tankshield (load-image "tankgui/tankshield.png")
   })

(def max-cell-size 128)
(def min-cell-size 32)

(defn- draw-cells
  [g board cell-size]
  (.setColor g Color/WHITE)
  (doseq [[[y x] val] board]
           (.drawImage g 
             (cond
               (= :wall val) (images :wall)
               (not (instance? clojure.lang.IReference val)) (images :other)
               (= :wall @val) (images :wall)
               (number? @val) (images :mine)
               (map? @val) (if (> (@val :shield) 0) (images :tankshield) (images :tank))
               true (images :other)
               )
             (inc (int (* x cell-size)))
             (inc (int (* y cell-size)))
             (dec cell-size)
             (dec cell-size)
             nil)
           (if (and (instance? clojure.lang.IReference val) (map? @val))
             (do
               (.setColor g Color/BLACK)
               (.drawString g (:name @val) (int (+ (* x cell-size) 5)) (int (+ (* y cell-size) 15)))
               ))))

(defn- draw-grid
  [g width height cell-size]
  (.setColor g Color/GRAY)
  (let [x1 0
        y1 0
        x2 (int (* cell-size width))
        y2 (int (* cell-size height))]
    (doseq [x (range x1 (inc x2) cell-size)]
      (.drawLine g x y1 x y2))
    (doseq [y (range y1 (inc y2) cell-size)]
      (.drawLine g x1 y x2 y))))

(defn- draw-grid-infinite
  [g cell-size]
  (let [r (.getClipBounds g)
        x1 (int (* (Math/floor (double (/ (.getX r) cell-size))) cell-size))
        y1 (int (* (Math/floor (double (/ (.getY r) cell-size))) cell-size))
        x2 (int (+ x1 (.getWidth r) cell-size))
        y2 (int (+ y1 (.getHeight r) cell-size))]
    (doseq [x (range x1 x2 cell-size)]
      (.drawLine g x y1 x y2))
    (doseq [y (range y1 y2 cell-size)]
      (.drawLine g x1 y x2 y))))

(defn- make-panel
  [viewer]
  (let [panel (proxy [JPanel] [] 
                (paint [g] 
                  (proxy-super paint g)
                  (.translate g @(viewer :x) @(viewer :y)) 
                  (if (or (= -1 @(viewer :width)) (= -1 @(viewer :height)))
                    (draw-grid-infinite g @(viewer :cell-size))
                    (draw-grid g @(viewer :width) @(viewer :height) @(viewer :cell-size)))
                  (draw-cells g @(viewer :board) @(viewer :cell-size))))
        cs @(viewer :cell-size)
        w @(viewer :width)
        h @(viewer :height)]
    (if (or (= -1 w) (= -1 h))
          (.setPreferredSize panel (new Dimension 800 600))
          (.setPreferredSize panel (new Dimension (* cs w) (* cs h)))
    )
    (.setBackground panel Color/WHITE)
    panel))

(def cell-renderer (proxy [javax.swing.ListCellRenderer]
    []
    (getListCellRendererComponent [list value index is-selected cell-has-focus]
          (let [panel (new JPanel)]
            (doto panel
                         (.setLayout (new BoxLayout panel BoxLayout/Y_AXIS))
                         (.add (new JLabel (str (:name value)))) 
                         (.add (new JLabel (str "Health: " (:health value)))) 
                         (.add (new JLabel (str "Energy: " (:energy value))))
                         (.setBorder (new BevelBorder BevelBorder/RAISED))
             (.setBackground (if is-selected Color/BLUE Color/WHITE)))
             panel))))

(defn- make-tank-panel
  [viewer]
  (doto (new JList)
    (.setModel (new DefaultListModel))
    (.setCellRenderer cell-renderer)
  ))


(defn make-viewer
  "Makes a new viewer that displays a board of given width and height, 
   or infinte width and height if no dimensions are supplied."
  ([cell-size width height]
  (let [viewer 
        {
        :board (atom {})
        :tanks (atom {})
        :display-panel (atom nil)
        :display-scroll (atom nil)
        :document (atom nil)
        :doc-scroll (atom nil)
        :tank-panel (atom nil)
        :frame (atom nil)
        :cell-size (atom cell-size)
        :x (atom 0)
        :y (atom 0)
        :width (atom width)
        :height (atom height)
        :mousex (atom 0)
        :mousey (atom 0)
        }
        panel (make-panel viewer)
        tank-panel (make-tank-panel viewer)
        frame (new JFrame "Tankland - A Tank Simulation in Clojure")
        scroll (new JScrollPane panel)
        console (new JEditorPane)
        doc-scroll (doto (new JScrollPane console) (.setMinimumSize (new Dimension 300 200)))
        splitPane (new JSplitPane JSplitPane/HORIZONTAL_SPLIT scroll (new JScrollPane tank-panel))
        vSplitPane (new JSplitPane JSplitPane/VERTICAL_SPLIT splitPane doc-scroll)
        document (new PlainDocument)]
  (reset! (viewer :display-panel) panel)
  (reset! (viewer :doc-scroll) doc-scroll)
  (reset! (viewer :tank-panel) tank-panel)
  (reset! (viewer :frame) frame)
  (reset! (viewer :display-scroll) scroll)
  (reset! (viewer :document) document)
  (.setDocument console document)
  (.setContinuousLayout splitPane true)
  (.setContinuousLayout vSplitPane true)
  (doto frame 
      (.setContentPane vSplitPane) 
      .pack 
      (.setVisible true))
  viewer))
  ([cell-size]
  (make-viewer cell-size -1 -1)))

(defn update-board
  "Updates the board of the viewer. Also calls a repaint."
  [viewer board]
  (reset! (viewer :board) board)
  (.repaint @(viewer :display-scroll)))

(defn update-tank-panel
  "Updates the tank information in the viewer. Also calls a repaint."
  [viewer tanks]
  (reset! (viewer :tanks) tanks)
  (let [tp @(viewer :tank-panel)
        lm (new DefaultListModel)]
    (doseq [[name tank] tanks]
      (.addElement lm @tank)
    )
    (.setModel tp lm)
  (.invalidate tp)
  (.repaint tp)))

(defn- do-action
  "the event handler for button actions"
  [e bf]
  )

(defn- do-slide-action
  "event handler for slider changes"
  [e viewer bf]
  (let [source (.getSource e)]
    (reset! (viewer :cell-size) (.getValue source))
    (.repaint @(viewer :display-scroll))
    (.setMainDividerLocation bf (.getWidth @(viewer :display-panel)))))

(defn create-board-frame
  "sets up the view"
  [panel viewer]
  (let [bf (boardFrame.)
        action-listener (proxy [java.awt.event.ActionListener] []
                          (actionPerformed [e] (do-action e bf)))
        change-listener (proxy [javax.swing.event.ChangeListener] []
                          (stateChanged [e] (do-slide-action e viewer bf)))]
    (.setGameBoard bf panel)
    (.addChangeListener bf change-listener)
    (.pack bf)
    (.setVisible bf true)
    ))
        

(defn init-graphics
  "Initilaizes the gui with a board of specified height and width."
  ([width height]
  (def ^:private viewervar (make-viewer 64 width height))
  (create-board-frame @(viewervar :display-scroll) viewervar)
  )
  ([size] (init-graphics size size))
  ([] (init-graphics -1 -1)))

(defn log-to-viewer
  "Logs a message to the console."
  [viewer message]
  (let [doc @(viewer :document)
        bar (.getVerticalScrollBar @(viewer :doc-scroll))]
    (.insertString doc (.getLength doc) (str message "\n") nil)
    (.setValue bar (.getMaximum bar))))

(defn log-message
  "Logs a message."
  [message]
  (log-to-viewer viewervar message))

(defn graphics-frame
  "Returns the graphics JFrame."
  []
  @(viewervar :frame))

(defn graphics-panel
  "Returns the graphics JFrame."
  []
  @(viewervar :display-panel))

(defn do-board
  "Updates the board."
  [board]
  (update-board viewervar board))

(defn do-tanks
  "Updates the tanks."
  [tanks]
  (update-tank-panel viewervar tanks))

(defn do-graphics
  "Updates the gui to show the current board."
  ([board]
  (update-board viewervar board))
  ([board tanks]
    (update-board viewervar board)
    (update-tank-panel viewervar tanks)))