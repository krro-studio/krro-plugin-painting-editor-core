(ns top.kzre.krro.plugin.painting.editor.core.overlay
  "Overlay 绘制：把瞬时的界面描述项画到图形上下文上。
   只依赖 IGraphicsContext 里已有的原语，因此任何 UI 框架实现都能承载。

   描述项统一为 [tag opts]，坐标是屏幕像素，颜色可以是 RGB float-array 或 [r g b a] 向量。
   每个方法只读取自己关心的键，缺省的键取 overlay/palette 里的默认配色，
   因此同一批 overlay 之间不会互相污染样式（每次绘制都包在 save!/restore! 里）。

     :cursor          工具光标   {:type :circle/:ring/:crosshair/:dot :x :y :radius :color :inner-color :stroke-width :gap}
     :circle          圆         {:x :y :radius :fill-color :stroke-color :stroke-width :stroke-dash :dash-offset}
     :rect            矩形       {:x :y :width :height} 或 {:bounds [x y w h]}，样式同上
     :line            直线       {:x1 :y1 :x2 :y2 :stroke-*}
     :polyline        折线       {:points [[x y]/{:x :y}...] :closed? :fill-color :stroke-*}
     :curve           贝塞尔路径 {:points [{:x :y :dx1 :dy1 :dx2 :dy2}...] :closed? :fill-color :stroke-*}
     :anchor          锚点       {:x :y :state :normal/:selected/:active/:hover :shape :square/:circle :size}
     :handle          手柄       {:x :y :hx :hy 或 :dx :dy :state :in/:out :size :line-color}
     :marquee         走马选区   {:type :rect/:ellipse/:polygon + :bounds/:points :dash-offset}
     :guide           参考线     {:x1 :y1 :x2 :y2 :label :label-x :label-y :color}
     :snap            吸附指示   {:type :anchor/:segment/:segment-center/:segment-perpendicular/:increment/:grid/:ruler :x :y :radius :angle}
     :transform-box   变换框     {:corners [[x y]...] 或 :x :y :width :height :angle :handles :corners/:bbox/:none :center? :handle-size}
     :text            文本       {:text :x :y :size :font :color :align :left/:center/:right :shadow?}"
  (:require
   [taoensso.timbre :as log]
   [top.kzre.krro.core.frame :as frame]
   [top.kzre.krro.core.reframe.core :as rf]
   [top.kzre.krro.plugin.painting.core.store :as store]
   [top.kzre.krro.plugin.painting.editor.core.graph :as graph]))

(defmulti draw-overlay!
          "根据描述项绘制到图形上下文。"
          (fn [desc _gc] (first desc)))

(defmethod draw-overlay! :default
  [desc _]
  (log/warn "Unknown overlay type:" (first desc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 默认配色与尺寸
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def palette
  "overlay 默认配色（0~1 的 [r g b a]）。描边普遍画两层：深色打底保证在浅色画布上可见，
   浅色内层保证在深色画布上可见——PS/CSP/SAI 的界面元素都是这个思路。"
  {:anchor          [0.13 0.13 0.13 1.0]
   :anchor-fill     [1.0 1.0 1.0 1.0]
   :anchor-selected [1.0 1.0 1.0 1.0]
   :anchor-active   [1.0 0.78 0.0 1.0]
   :anchor-hover    [0.35 0.65 1.0 0.85]
   :handle          [0.20 0.20 0.20 1.0]
   :handle-fill     [0.35 0.78 0.35 1.0]
   :handle-line     [0.55 0.55 0.55 0.9]
   :path            [0.13 0.13 0.13 1.0]
   :path-preview    [0.85 0.35 0.15 1.0]
   :marquee-dark    [0.0 0.0 0.0 1.0]
   :marquee-light   [1.0 1.0 1.0 1.0]
   :guide           [0.0 0.72 0.92 1.0]
   :snap            [1.0 0.78 0.0 1.0]
   :snap-ring       [1.0 1.0 1.0 0.9]
   :box             [0.0 0.0 0.0 0.85]
   :box-inner       [1.0 1.0 1.0 0.7]
   :box-handle      [1.0 1.0 1.0 1.0]
   :text            [1.0 1.0 1.0 1.0]
   :text-shadow     [0.0 0.0 0.0 0.75]
   :cursor          [0.0 0.0 0.0 1.0]
   :cursor-inner    [1.0 1.0 1.0 1.0]})

(def ^:private line-width 1.0)
(def ^:private anchor-half 4.0)
(def ^:private handle-half 3.0)
(def ^:private box-handle-half 4.0)
(def ^:private snap-radius 5.0)
(def ^:private text-size 12.0)
(def ^:private ants-dash [4.0 4.0])

;; 没有文本度量原语，对齐只能按字号估算字宽；等宽字体下误差很小。
(def ^:private char-width-ratio 0.6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 内部工具
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-state
  "隔离单次绘制的样式状态，避免描述项之间互相残留。"
  [gc & body]
  `(do (graph/save! ~gc)
       (try
         ~@body
         (finally (graph/restore! ~gc)))))

(defn- kw
  "关键字化 v；nil 或其它类型取 default。"
  [v default]
  (cond
    (keyword? v) v
    (or (string? v) (symbol? v)) (keyword v)
    :else default))

(defn- point->xy [p]
  (cond
    (map? p)    [(double (or (:x p) 0.0)) (double (or (:y p) 0.0))]
    (vector? p) [(double (nth p 0 0.0)) (double (nth p 1 0.0))]
    :else       [0.0 0.0]))

(defn- bounds->rect [{:keys [bounds x y width height]}]
  (if-let [[bx by bw bh] bounds]
    [(double bx) (double by) (double bw) (double bh)]
    [(double (or x 0.0)) (double (or y 0.0)) (double (or width 0.0)) (double (or height 0.0))]))

(defn- stroke-style!
  "套用描边样式；defaults 形如 {:stroke-color [...] :stroke-width 1.0 :stroke-dash [...]}。"
  [gc opts defaults]
  (graph/set-stroke-color! gc (or (:stroke-color opts) (:stroke-color defaults)))
  (graph/set-stroke-width! gc (double (or (:stroke-width opts) (:stroke-width defaults) line-width)))
  (if-let [dash (or (:stroke-dash opts) (:stroke-dash defaults))]
    (do (graph/set-stroke-dash! gc dash)
        (graph/set-stroke-dash-offset! gc (double (or (:dash-offset opts) 0.0))))
    (graph/set-stroke-dash! gc [])))

(defn- trace-poly! [gc points closed?]
  (let [[x0 y0] (point->xy (first points))]
    (graph/begin-path! gc)
    (graph/move-to! gc x0 y0)
    (doseq [p (next points)]
      (let [[x y] (point->xy p)]
        (graph/line-to! gc x y)))
    (when closed? (graph/close-path! gc))))

(defn- trace-curve! [gc points closed?]
  ;; 控制点带切向手柄：:dx2/:dy2 是出发侧，:dx1/:dy1 是到达侧（相对各自锚点）。
  (let [cs (mapv (fn [p]
                   (let [[x y] (point->xy p)]
                     {:x x :y y
                      :out-x (+ x (double (or (:dx2 p) 0.0)))
                      :out-y (+ y (double (or (:dy2 p) 0.0)))
                      :in-x  (+ x (double (or (:dx1 p) 0.0)))
                      :in-y  (+ y (double (or (:dy1 p) 0.0)))}))
                 points)
        n (count cs)
        segs (if closed? n (dec n))]
    (graph/begin-path! gc)
    (graph/move-to! gc (:x (first cs)) (:y (first cs)))
    (dotimes [i segs]
      (let [a (cs i) b (cs (mod (inc i) n))]
        (graph/bezier-curve-to! gc (:out-x a) (:out-y a) (:in-x b) (:in-y b) (:x b) (:y b))))
    (when closed? (graph/close-path! gc))))

(defn- fill-then-stroke-path!
  "先填充（可选）再描边——填充会盖住描边的内侧。"
  [gc opts]
  (when-let [fc (:fill-color opts)]
    (graph/set-fill-color! gc fc)
    (graph/fill-path! gc))
  (graph/stroke-path! gc))

(defn- marker!
  "锚点/手柄端的小标记：先填充再描边。"
  [gc shape x y half fill-color stroke-color stroke-width]
  (let [h (double half)
        x (double x) y (double y)]
    (graph/set-stroke-dash! gc [])
    (graph/set-fill-color! gc fill-color)
    (graph/set-stroke-color! gc stroke-color)
    (graph/set-stroke-width! gc (double stroke-width))
    (if (= shape :circle)
      (do (graph/fill-oval! gc x y h h)
          (graph/draw-oval! gc x y h h))
      (do (graph/fill-rect! gc (- x h) (- y h) (* 2.0 h) (* 2.0 h))
          (graph/draw-rect! gc (- x h) (- y h) (* 2.0 h) (* 2.0 h))))))

(defn- ring!
  "双层圆环：深色外环 + 可选浅色内环，内环在半径太小时省略。"
  [gc x y radius stroke-color stroke-width inner-color]
  (let [r (double radius)
        w (double stroke-width)]
    (graph/set-stroke-dash! gc [])
    (graph/set-stroke-width! gc w)
    (graph/set-stroke-color! gc stroke-color)
    (graph/draw-oval! gc x y r r)
    (when (and (some? inner-color) (> r (* 2.0 w)))
      (graph/set-stroke-color! gc inner-color)
      (graph/draw-oval! gc x y (- r w) (- r w)))))

(defn- crosshair!
  "四段带中心缺口的十字线。"
  [gc x y length gap stroke-color stroke-width]
  (let [l (double length)
        g (double gap)]
    (graph/set-stroke-dash! gc [])
    (graph/set-stroke-color! gc stroke-color)
    (graph/set-stroke-width! gc (double stroke-width))
    (doseq [[x1 y1 x2 y2] [[(- x l) y (- x g) y]
                           [(+ x g) y (+ x l) y]
                           [x (- y l) x (- y g)]
                           [x (+ y g) x (+ y l)]]]
      (graph/draw-line! gc x1 y1 x2 y2))))

(defn- perpendicular-bar!
  "过 (x y)、与方向角 angle（弧度）垂直的一小段标记线。"
  [gc x y angle len]
  (let [a (double angle)
        h (/ (double len) 2.0)
        nx (- (Math/sin a))
        ny (Math/cos a)]
    (graph/draw-line! gc (- x (* h nx)) (- y (* h ny))
        (+ x (* h nx)) (+ y (* h ny)))))

(defn- rotated-corners [x y width height angle]
  (let [cx (+ (double x) (/ width 2.0))
        cy (+ (double y) (/ height 2.0))
        a (double (or angle 0.0))
        sa (Math/sin a)
        ca (Math/cos a)
        hw (/ (double width) 2.0)
        hh (/ (double height) 2.0)]
    (mapv (fn [[dx dy]]
            [(+ cx (- (* dx ca) (* dy sa)))
             (+ cy (+ (* dx sa) (* dy ca)))])
          [[(- hw) (- hh)] [hw (- hh)] [hw hh] [(- hw) hh]])))

(defn- edge-mids [corners]
  (let [cs (vec corners)]
    (when (> (count cs) 1)
      (map (fn [[x1 y1] [x2 y2]] [(/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)])
           cs
           (concat (rest cs) [(first cs)])))))

(defn- handle-positions [corners mode]
  (case (kw mode :bbox)
    :none    []
    :corners (take 4 corners)
    (concat (take 4 corners) (edge-mids corners))))

(defn- stroke-shape!
  "按形状描一次边（走马线要用两种样式各描一次）。"
  [gc shape opt]
  (case (kw shape :rect)
    :ellipse (let [[x y w h] (bounds->rect opt)]
               (graph/draw-oval! gc (+ x (/ w 2.0)) (+ y (/ h 2.0)) (/ w 2.0) (/ h 2.0)))
    :polygon (do (trace-poly! gc (:points opt) true) (graph/stroke-path! gc))
    (let [[x y w h] (bounds->rect opt)]
      (graph/draw-rect! gc x y w h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 绘制方法
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod draw-overlay! :cursor
  [[_ {:keys [type x y radius color inner-color stroke-width gap] :or {type :circle}}] gc]
  (with-state gc
    (let [r (double (or radius 8.0))
          w (double (or stroke-width line-width))
          c (or color (:cursor palette))]
      (case (kw type :circle)
        ;; 画笔光标：圆环 + 穿过圆心的十字（SAI/PS 的精确光标），:gap 可选
        :circle (do (ring! gc x y r c w inner-color)
                    (crosshair! gc x y r (or gap 0.0) c w))
        :ring   (ring! gc x y r c w (or inner-color (:cursor-inner palette)))
        :crosshair (crosshair! gc x y r (or gap (/ r 4.0)) c w)
        :dot    (let [d (max 1.0 w)]
                  (graph/set-stroke-dash! gc [])
                  (graph/set-fill-color! gc c)
                  (graph/fill-rect! gc (- x (/ d 2.0)) (- y (/ d 2.0)) d d))
        nil))))

(defmethod draw-overlay! :circle
  [[_ {:keys [x y radius] :as opt}] gc]
  (with-state gc
    (let [r (double radius)]
      (stroke-style! gc opt {:stroke-color (:path palette)})
      (when-let [fc (:fill-color opt)]
        (graph/set-fill-color! gc fc)
        (graph/fill-oval! gc x y r r))
      (graph/draw-oval! gc x y r r))))

(defmethod draw-overlay! :rect
  [[_ opt] gc]
  (with-state gc
    (let [[x y w h] (bounds->rect opt)]
      (stroke-style! gc opt {:stroke-color (:path palette)})
      (when-let [fc (:fill-color opt)]
        (graph/set-fill-color! gc fc)
        (graph/fill-rect! gc x y w h))
      (graph/draw-rect! gc x y w h))))

(defmethod draw-overlay! :line
  [[_ {:keys [x1 y1 x2 y2] :as opt}] gc]
  (with-state gc
    (stroke-style! gc opt {:stroke-color (:path palette)})
    (graph/draw-line! gc x1 y1 x2 y2)))

(defmethod draw-overlay! :polyline
  [[_ {:keys [points closed?] :as opt}] gc]
  (with-state gc
    (when (next points)
      (stroke-style! gc opt {:stroke-color (:path palette)})
      (trace-poly! gc points (boolean closed?))
      (fill-then-stroke-path! gc opt))))

(defmethod draw-overlay! :curve
  [[_ {:keys [points closed?] :as opt}] gc]
  (with-state gc
    (when (next points)
      (stroke-style! gc opt {:stroke-color (:path palette)})
      (trace-curve! gc points (boolean closed?))
      (fill-then-stroke-path! gc opt))))

(defn- anchor-colors [state]
  (case (kw state :normal)
    :selected [(:anchor-selected palette) (:anchor palette)]
    :active   [(:anchor-active palette) (:anchor palette)]
    :hover    [(:anchor-hover palette) (:anchor-hover palette)]
    [(:anchor-fill palette) (:anchor palette)]))

(defmethod draw-overlay! :anchor
  [[_ {:keys [x y state shape size stroke-width]}] gc]
  (with-state gc
    (let [[fill stroke] (anchor-colors state)]
      (marker! gc (kw shape :square) x y (or size anchor-half)
               fill stroke (or stroke-width line-width)))))

(defmethod draw-overlay! :handle
  [[_ {:keys [x y hx hy dx dy state size line-color]}] gc]
  (with-state gc
    (let [tx (double (or hx (if (some? dx) (+ (double x) dx) x)))
          ty (double (or hy (if (some? dy) (+ (double y) dy) y)))
          out? (not= (kw state :out) :in)]
      (graph/set-stroke-dash! gc [])
      (graph/set-stroke-color! gc (or line-color (:handle-line palette)))
      (graph/set-stroke-width! gc line-width)
      (graph/draw-line! gc x y tx ty)
      ;; 伸出侧用空心方块、收回侧用实心圆，便于分辨手柄归属
      (marker! gc (if out? :square :circle) tx ty (or size handle-half)
               (if out? (:anchor-fill palette) (:handle-fill palette))
               (:handle palette) line-width))))

(defmethod draw-overlay! :marquee
  [[_ {:keys [type dash-offset] :as opt}] gc]
  (with-state gc
    ;; 深色实线打底 + 浅色虚线覆盖：任何背景都能看清边界
    (graph/set-stroke-color! gc (:marquee-dark palette))
    (graph/set-stroke-width! gc line-width)
    (graph/set-stroke-dash! gc [])
    (stroke-shape! gc type opt)
    (graph/set-stroke-color! gc (:marquee-light palette))
    (graph/set-stroke-dash! gc ants-dash)
    (graph/set-stroke-dash-offset! gc (double (or dash-offset 0.0)))
    (stroke-shape! gc type opt)))

(defmethod draw-overlay! :guide
  [[_ {:keys [x1 y1 x2 y2 label label-x label-y color] :as opt}] gc]
  (with-state gc
    (stroke-style! gc opt {:stroke-color (or color (:guide palette))})
    (graph/draw-line! gc x1 y1 x2 y2)
    (when (some? label)
      (draw-overlay! [:text {:text label
                             :x (or label-x (+ (double x1) 4.0))
                             :y (or label-y (- (double y1) 3.0))
                             :color (:guide palette)
                             :shadow? true}] gc))))

(defmethod draw-overlay! :snap
  [[_ {:keys [type x y radius angle color]}] gc]
  (with-state gc
    (let [r (double (or radius snap-radius))
          c (or color (:snap palette))]
      (case (kw type :anchor)
        (:segment :segment-center :segment-perpendicular)
        (if (some? angle)
          (do (graph/set-stroke-dash! gc [])
              (graph/set-stroke-color! gc c)
              (graph/set-stroke-width! gc 1.5)
              (perpendicular-bar! gc x y angle (* 2.4 r)))
          (ring! gc x y r (:snap-ring palette) line-width c))
        (:increment :grid :ruler)
        (crosshair! gc x y (* 1.8 r) r c line-width)
        (ring! gc x y r (:snap-ring palette) line-width c)))))

(defmethod draw-overlay! :transform-box
  [[_ {:keys [corners angle handles handle-size center?] :as opt}] gc]
  (with-state gc
    (let [[bx by bw bh] (bounds->rect opt)
          cs (or corners (rotated-corners bx by bw bh angle))
          hs (handle-positions cs (kw handles :bbox))
          half (double (or handle-size box-handle-half))
          cx (/ (reduce + (map first cs)) (double (count cs)))
          cy (/ (reduce + (map second cs)) (double (count cs)))]
      (graph/set-stroke-dash! gc [])
      (graph/set-stroke-width! gc line-width)
      (graph/set-stroke-color! gc (:box palette))
      (trace-poly! gc cs true)
      (graph/stroke-path! gc)
      (when (= (kw handles nil) :rotate)
        (graph/set-stroke-color! gc (:box-inner palette))
        (graph/set-stroke-dash! gc ants-dash)
        (let [[a b c d] cs]
          (doseq [[p q] [[a c] [b d]]]
            (graph/draw-line! gc (first p) (second p) (first q) (second q))))
        (graph/set-stroke-dash! gc []))
      (when center?
        (crosshair! gc cx cy (* 2.0 half) half (:box palette) line-width))
      (doseq [[hx hy] hs]
        (marker! gc :square hx hy half (:box-handle palette) (:box palette) line-width)))))

(defmethod draw-overlay! :text
  [[_ {:keys [text x y size font color align shadow?] :or {shadow? true}}] gc]
  (with-state gc
    (let [s (str text)
          sz (double (or size text-size))]
      (graph/set-font! gc (or font (str sz "px Sanserif")))
      (let [dx (case (kw align :left)
                 :center (* -0.5 char-width-ratio sz (count s))
                 :right  (* (- char-width-ratio) sz (count s))
                 0.0)]
        (when shadow?
          (graph/set-fill-color! gc (:text-shadow palette))
          (graph/draw-text! gc s (+ (double x) dx 1.0) (+ (double y) 1.0)))
        (graph/set-fill-color! gc (or color (:text palette)))
        (graph/draw-text! gc s (+ (double x) dx) (double y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 刷新
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rf/reg-fx
  store/app-id :tool/flush-overlay
  (fn [_ overlay-v frame]
    (when-let [gc (frame/param frame :krro.painting/overlay-graph-context)]
      (graph/submit! gc
                     (fn []
                       (graph/clear! gc)
                       (doseq [overlay (or overlay-v [])]
                         (draw-overlay! overlay gc)))))))
