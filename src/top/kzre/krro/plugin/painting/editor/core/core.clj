(ns top.kzre.krro.plugin.painting.editor.core.core
  "绘图插件入口。"
  (:require
   [top.kzre.krro.canvas.core.core]
   [top.kzre.krro.canvas.vector.core]
   [top.kzre.krro.core.core :as krro]
   [top.kzre.krro.plugin.painting.core.core]
   [top.kzre.krro.plugin.painting.editor.core.mode.painting :as painting]
   [top.kzre.krro.plugin.painting.editor.core.mode.basic :as basic]
   [top.kzre.krro.plugin.painting.editor.core.mode.color-picker-hsv-rect :as color-picker-hsv-rect]
   [top.kzre.krro.plugin.painting.editor.core.mode.color-picker-wheel-hsv :as color-picker-wheel-hsv]
   [top.kzre.krro.plugin.painting.editor.core.mode.new-image :as new-image]
   [top.kzre.krro.plugin.painting.editor.core.overlay]
   [top.kzre.krro.plugin.undo.core]
   [top.kzre.krro.ui.javafx.core]))


(krro/reg-plugin!
  {:name :krro.painting/editor-core
   :mount
   (fn []
     (painting/mount)
     (basic/mount)
     (new-image/mount)
     (color-picker-wheel-hsv/mount)
     (color-picker-hsv-rect/mount)
     )})