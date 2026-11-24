(ns top.kzre.krro.plugin.painting.editor.core.icon
  "图标资源管理., key是
  [:light/:dark icon-id]"
  (:require
   [top.kzre.krro.core.util.router :as router]
   [top.kzre.krro.ui.styles.core :as styles]))

(router/defrouter
  icon-router
  {
   [:light :eye true]
   "classpath:/resources/krro/plugin/painting/editor/core/icons/eye.light.png"
   [:light :eye false]
   "classpath:/resources/krro/plugin/painting/editor/core/icons/eye-off.light.png"
   [:light :lock true]
   "classpath:/resources/krro/plugin/painting/editor/core/icons/lock.light.png"
   [:light :lock false]
   "classpath:/resources/krro/plugin/painting/editor/core/icons/lock-open.light.png"})

(defn- url* [path]
  (router/resolve icon-router path))

(def url (memoize url*))

(defn light-dark
  "明暗模式切换的图标."
  [path]
  (let [light-mode (styles/light-mode)
        full-path (vec (cons light-mode path))]
    (url full-path)))