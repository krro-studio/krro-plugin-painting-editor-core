(ns top.kzre.krro.plugin.painting.editor.core.ui.tool-bar
  (:require
    [top.kzre.krro.plugin.painting.core.edit.tools :as tools]
    [top.kzre.krro.plugin.painting.core.store :as store]))

(defn tool-bar-vnode [canvas-id frame]
  (let [current-tool @(store/subscribe canvas-id :current-tool)]
    (into [:tool-bar {:class ["tool-bar"]
                      :max-height 50}]
          (for [{:keys [id name icon]} (tools/tools)]
            [:button {:key id
                      :class (str "tool-btn" (when (= id current-tool) " active"))
                      :content (str icon " " name)
                      :on-click
                      (fn [_]
                        ;; 选择工具
                        (store/dispatch [:tool/select-tool canvas-id id])
                        ;; 刷新UI
                        (store/dispatch [:tool/flush-overlay canvas-id nil frame]))}]))))