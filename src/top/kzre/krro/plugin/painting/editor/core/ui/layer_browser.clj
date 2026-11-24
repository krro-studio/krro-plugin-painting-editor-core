(ns top.kzre.krro.plugin.painting.editor.core.ui.layer-browser
  "图层面板：标题、图层列表（递归树结构，支持拖拽排序）和工具栏。"
  (:require
   [taoensso.timbre :as log]
   [top.kzre.krro.canvas.core.layer.core :as layer.core]
   [top.kzre.krro.canvas.core.layer.group :as layer.group]
   [top.kzre.krro.canvas.core.layer.path :as layer.path]
   [top.kzre.krro.plugin.painting.core.ops.layer-undo :as layer-undo]
   [top.kzre.krro.plugin.painting.core.project.canvas :as pc]
   [top.kzre.krro.plugin.painting.core.project.layer :as project.layer]
   [top.kzre.krro.plugin.painting.core.store :as store]
   [top.kzre.krro.plugin.painting.editor.core.icon :as icon]
   [top.kzre.krro.ui.core.spec.drag :as drag-spec]))

(defrecord LayerRowInfo
  [layer-id                                                       ;; 图层id
   layer-name                                                     ;; 图层名称
   ^boolean visible?                                        ;; 图层可见性
   ^boolean group-visible?                                 ;; 图层所在组可见性
   ^boolean locked?                                         ;; 图层是否锁定
   ^boolean group-locked?                                   ;; 图层所在组是否锁定
   ^boolean alpha-locked?                                   ;; 图层透明度是否锁定
   ^boolean group-alpha-locked?                             ;; 图层所在组的透明度是否锁定
   ^long level                                              ;; 图层层级
   layer                                                    ;; 图层引用, 渲染图层特定信息时候取出它来
   children
   ])

;; ── 构建树结构（保留 LayerRowInfo，children 作为额外字段挂在上面）──
(defn- build-layer-tree
  ([layers]
   (build-layer-tree layers 0 nil))
  ([layers level group]
   (mapv
     (fn [layer]
       (let [info (map->LayerRowInfo
                    {:layer-id (:id layer)
                     :layer-name (project.layer/layer-name layer)
                     :layer layer
                     :level level
                     :visible? (layer.core/visible? layer)
                     :group-visible? (layer.core/visible? group)
                     :locked? (project.layer/locked? layer)
                     :group-locked? (project.layer/locked? group)
                     :alpha-locked? (project.layer/alpha-locked? layer)
                     :group-alpha-locked? (project.layer/alpha-locked? group)})]
         (if (layer.group/group? layer)
           (let [children (build-layer-tree (:layers layer) (inc level) layer)]
             (assoc info :children children))
           info)))
     ;; 图层是栈, 显示方向是反向的
     (reverse layers))))

;; ── 图层行 vnode（只管自己，缩进交给父容器）──────────
(defn- layer-row
  "结构划分为
  row {<left> [name] <flags>}"
  [^LayerRowInfo info canvas-id
   {:keys [current-layer-id selected-layer-ids]
    :as _params}]
  (let [{:keys [layer-id layer-name layer visible? locked?]} info
        current? (= layer-id current-layer-id)
        selected? (contains? selected-layer-ids layer-id)]
    [:block {:key layer-id
             :direction :horizontal
             :grow true
             :class ["layer-row"]
             ;; 拖拽源
             :on-drag
             (drag-spec/drag-source
               (fn [_]
                 (log/info "get drag content")
                 (str layer-id))
               :modes [:move])
             ;; 拖拽目标
             :on-drop (drag-spec/drag-target
                        (fn [node e] true)
                        (fn [node e]
                          (let [source-id (keyword (:data e))
                                target-id layer-id
                                layers (pc/layers-by-id! canvas-id)
                                source-path (layer.path/get-path layers source-id)
                                target-path (layer.path/get-path layers target-id)]
                            (when (and source-path target-path (not= source-id target-id))
                              (layer-undo/move-layer-undo! canvas-id source-path target-path)))))
             }
     ;; 可见性
     [:image {:src (icon/light-dark [:eye visible?])
              :width 20
              :on-click
              (fn [_]
                (log/info "on click")
                (store/dispatch
                  [:oplog/toggle-layer-visibility
                   canvas-id layer-id
                   {:undo true}]))}]
     ;; 组展开/折叠图标
     (when (layer.group/group? layer)
       [:image {:src "classpath:/resources/krro/plugin/painting/editor/core/icons/caret-right.light.png"
                :width 20
                :on-click
                (fn [_]
                  (log/info "expand group")
                  )}])
     ;; 名称区域
     [:block {:direction :horizontal
              :grow true
              :class
              (cond
                selected?
                ["layer-name-box" "layer-name-box-selected"]
                :else ["layer-name-box"])
              :on-click
              (fn [{:keys [shift?]}]
                (if shift?
                  (store/dispatch [:select-layer-multi canvas-id layer-id])
                  (store/dispatch [:select-layer canvas-id layer-id])))}
      ;; 图层名称
      [:text {:content layer-name
              :class ["layer-name-text"]}]
      ;; 挤占右侧位置
      [:block {:grow true :style {:min-width "8px"}}]]
     ;; 右侧面板
     ;; 锁定
     [:image {:src (icon/light-dark [:lock locked?])
              :width 20
              :on-click
              (fn [_]
                (log/info "toggle layer lock")
                (store/dispatch
                  [:oplog/toggle-layer-lock
                   canvas-id layer-id
                   {:undo true}]))}]]))

;; ── 递归渲染树节点 ──────────────────────────────────
(defn- render-layer-node
  [info canvas-id params]
  (let [{:keys [layer-id children]} info]
    (if children
      ;; 组节点：标题行 + 嵌套 children
      [:block {:key layer-id
               :direction :vertical
               :class ["layer-group"]}
       (layer-row info canvas-id params)
       ;; 子图层缩进容器
       [:block {:direction :vertical
                :class ["layer-group-child"]
                :style {:margin-left "12px"}}
        (mapv #(render-layer-node % canvas-id params) children)]]
      ;; 叶子节点
      (layer-row info canvas-id params))))

;; ── 图层面板 ────────────────────────────────────────
(defn layer-browser [canvas-id frame]
  (let [layers      @(store/subscribe canvas-id :layers)
        current-layer-id @(store/subscribe canvas-id :current-layer-id)
        selected-layer-ids @(store/subscribe canvas-id :selected-layer-ids)
        params
        {:current-layer-id current-layer-id
         :selected-layer-ids selected-layer-ids}
        tree        (build-layer-tree layers)]
    [:block
     {:direction :vertical
      :stylesheet
      "classpath:/resources/krro/plugin/painting/editor/core/stylesheets/layer_browser.dark.edn"
      :class ["layer-browser"]}
     [:text {:class "layer-browser-title"
             :content "Layers"}]
     ;; 图层列表
     [:scroll {:class ["layer-list"]}
      (into [:block
             {:direction :vertical
              :grow true}]
            (mapv #(render-layer-node %
                                      canvas-id
                                      params) tree))]
     ;; 工具栏
     [:block {:class "layer-toolbar"
              :direction :horizontal
              :spacing "4px"}
      [:krro.painting/add-layer-popup {:krro.painting/canvas-id canvas-id}]
      [:image {:src "classpath:/resources/krro/plugin/painting/editor/core/icons/x.light.png"
               :width 20
               :on-click
               (fn [_]
                 (log/info "on-click"))}]
      [:button {:class "layer-toolbar-button" :content "dup"
                :on-click
                (fn [_]
                  (layer-undo/duplicate-selected-layer-undo! canvas-id))}]]]))