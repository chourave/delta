(ns plumula.delta.spec.helper
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string])
  #?(:cljs (:require-macros [plumula.delta.spec.helper])))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def text
  ""
  (s/and string? #(-> % count pos?)))

(def embed
  ""
  (s/map-of keyword? any? :kind #(= (count %) 1)))

(def attributes
  ""
  (s/map-of keyword? any?))

(def valued-attributes
  ""
  (s/and attributes
         #(every? (comp some? val) %)))

(defn cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))

(defmacro s-or
  ""
  [& args]
  `(if-cljs
     (cljs.spec.alpha/or ~@args)
     (clojure.spec.alpha/or ~@args)))

(defmacro s-and
  ""
  [& args]
  `(if-cljs
     (cljs.spec.alpha/and ~@args)
     (clojure.spec.alpha/and ~@args)))

(defmacro guard
  "Specifies that, when `test` is true, then `then` must true. If an optional
  `else` is provided, then `else` must be true when `test` is false.
  `then-label` and `else-label` should be keywords used for labeling the specs.
  "
  ([test then]
   `(guard ~test :then ~then :else (constantly true)))
  ([test then-label then else-label else]
   `(s-or
      ~then-label (s-and ~test ~then)
      ~else-label (s-and (complement ~test) ~else))))
