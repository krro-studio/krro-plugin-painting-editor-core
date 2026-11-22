(ns top.kzre.krro.plugin.painting.editor.core.mode.painting
  "单张画布的绘制, 插画，漫画走这个流程。绘制数据直接进入 CanvasData."
  (:require
    [top.kzre.krro.core.core :as core]
    [top.kzre.krro.core.frame :as frame]
    [top.kzre.krro.plugin.painting.core.spec :as spec]
    [top.kzre.krro.plugin.painting.core.store :as store]
    [top.kzre.krro.plugin.painting.editor.core.ui.layer-browser :as lb]
    [top.kzre.krro.plugin.painting.editor.core.ui.menu-bar :as menu-bar]
    [top.kzre.krro.plugin.painting.editor.core.ui.tool-bar :as tb]
    [top.kzre.krro.ui.core.core :as krro.ui]))

;; TODO 场外通信
(def ^:private toggle-tool-bar-key ::toggle-tool-bar)

(defn ensure-canvas [state-atom canvas-id]
  (let [[old _] (swap-vals! state-atom assoc :canvas-id canvas-id)]
    (if-let [old-canvas-id (:canvas-id old)]
      (when-not (= old-canvas-id canvas-id)
        ;; 切换资源
        (store/unreg-canvas-store canvas-id)
        (when canvas-id
          (store/reg-canvas-store canvas-id)))
      ;; 创建资源
      (when canvas-id
        (store/reg-canvas-store canvas-id)))))

(defn- component [_]
  (let [state-atom  (atom {})]
    {:on-unmount
     (fn [_]
       ;; 画布资源，由SPA 组件管理
       (ensure-canvas state-atom nil))
     :render
     (fn [_ frame]
       ;; 每个Frame 只可能出现单个 Canvas
       (let [canvas-id (frame/param frame spec/canvas-id-key)]
         (ensure-canvas state-atom canvas-id)
         (if canvas-id
           [:block {:direction :vertical}
            (menu-bar/menu-bar frame)
            [:split {:direction :vertical
                     :grow      true}
             ;; 顶部工具选择栏
             (tb/tool-bar-vnode canvas-id frame)
             [:split {:direction :horizontal
                      :grow      true}
              ;; 左侧画布
              [:krro.painting/canvas {:krro.painting/canvas-id canvas-id}]
              ;; 右侧图层面板
              (lb/layer-panel-vnode canvas-id frame)]]]
           [:text :content "Invalid Canvas State."])
         ))}))


(defn mount
  []
  (krro.ui/reg-component ::component component)
  (core/define-major-mode
    :krro.painting/painting
    :layout [::component]
    :keymap
    {:l :krro.painting/log-layers
     :s :krro.painting.anchor/enter-adjust-width-modal
     :e :krro.painting.anchor/enter-extrude-anchor-modal
     :x :krro.painting/delete-selected-anchors})
)