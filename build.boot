(def project 'trek)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "1.9.0-alpha16"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [instaparse "1.4.7"]
                            [org.clojure/core.async "0.3.443"]
                            [org.clojure/core.match "0.3.0-alpha5"]
                            [org.clojure/tools.nrepl "0.2.12" :exclude [org.clojure/clojure]]
                            [cider/cider-nrepl "0.15.1-SNAPSHOT"]
                            [refactor-nrepl "2.4.0-SNAPSHOT"]
                            [org.clojure/tools.namespace "0.2.11"]
                            [clj-time "0.14.0"]])

(require '[cider.tasks :refer [add-middleware]])

(task-options! add-middleware {:middleware '[cider.nrepl.middleware.apropos/wrap-apropos
                                             cider.nrepl.middleware.version/wrap-version]})

(task-options!
 aot {:namespace   #{'trek.core}}
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/trek"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}}
 jar {:main        'trek.core
      :file        (str "trek-" version "-standalone.jar")})

(deftask build
  "Build the project locally as a JAR."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp (aot) (pom) (uber) (jar) (target :dir dir))))

(deftask run
  "Run the project."
  [a args ARG [str] "the arguments for the application."]
  (require '[trek.core :as app])
  (apply (resolve 'app/-main) args))
