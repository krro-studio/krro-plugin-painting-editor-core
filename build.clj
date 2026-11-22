(ns build
  (:require
    [top.kzre.clj-build-template.api :as tplb]
    [top.kzre.krro.ui.javafx.tool.compile-stylesheet :as kfx]
    [top.kzre.clj-build-template.simple :as tpl]))

(def ^:private stylesheet-classpath-root
  "resources/resources/krro/plugin/painting/editor/core/stylesheets")

(defn stylesheet-classpath [filename]
      (str stylesheet-classpath-root "/" filename))

(def params {:template (tpl/create-template)
             :artifact "krro-plugin-painting-editor-core"
             :version "0.1.0"
             :resource-dirs ["resources"]})

(defn clean [_]
      (tpl/clean)
      (tplb/delete-dir
        ;; 删除所有编译后而样式文件
        {:target-dir stylesheet-classpath-root
         :include (tplb/exts->include-glob ["css"])}))

(defn compile-stylesheet [_]
      (kfx/compile-stylesheet
        {:src-files [(stylesheet-classpath "layer_browser.dark.edn")]
         :output-path (stylesheet-classpath "layer_browser.dark.css")} ))

(defn jar [_]
      (tpl/jar* params))

(defn install [_]
      (tpl/install params))
