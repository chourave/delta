; MIT License
;
; Copyright (c) 2017 Frederic Merizen
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(ns plumula.delta.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [plumula.delta :as delta]
            [plumula.delta.operation :as operation]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(def text (s/and string? #(-> % count pos?)))
(def embed (s/map-of keyword? any? :kind #(= (count %) 1)))
(def attributes (s/map-of keyword? any?))

(s/def ::delta/insert (s/or :text text :embed embed))
(s/def ::delta/delete pos-int?)
(s/def ::delta/retain pos-int?)
(s/def ::delta/attributes (s/and attributes seq))

(defmulti operation-spec #(when (map? %) (operation/type %)))

(defmethod operation-spec ::delta/insert [_]
  (s/keys :req [::delta/insert] :opt [::delta/attributes]))

(defmethod operation-spec ::delta/delete [_]
  (s/keys :req [::delta/delete]))

(defmethod operation-spec ::delta/retain [_]
  (s/keys :req [::delta/retain] :opt [::delta/attributes]))

(s/def ::delta/operation (s/multi-spec operation-spec (fn [o _] o)))

(defn- unconform-operation
  "Given `conformed-operation`, the result of of conforming a value to
  `::delta/operation`, returns a raw operation.
  "
  [conformed-operation]
  (if (::delta/insert conformed-operation)
    (update conformed-operation ::delta/insert val)
    conformed-operation))

(defn- conformed-operation-length
  "Given `conformed-operation`, the result of of conforming a value to
  `::delta/operation`, returns the operation’s length in characters.
  "
  [conformed-operation]
  (-> conformed-operation
      unconform-operation
      operation/length))

(s/def ::delta/delta (s/coll-of ::delta/operation))

(s/fdef operation/length
  :args (s/cat :operation ::delta/operation)
  :ret pos-int?)

(s/fdef operation/type
  :args (s/cat :operation ::delta/operation)
  :ret #{::delta/insert ::delta/delete ::delta/retain})

(defn- operation-type-preserved?
  "True if the instrumented function returned an operation with the
  same type as its `operation` argument.
  "
  [{:keys [args ret]}]
  (= (-> ret operation/type)
     (-> args :operation operation/type)))

(defn- attributes-preserved?
  "True if the instrumented function returned an operation with the
  same attributes as its `operation` argument.
  "
  [{:keys [args ret]}]
  (= (-> ret ::delta/attributes)
     (-> args :operation ::delta/attributes)))

(defn- operation-shortenend-to-n?
  "True if the instrumented function shortened its `operation` argument
  to `n` characters (`n` is also an argument).
  "
  [{:keys [args ret]}]
  (= (-> ret conformed-operation-length)
     (min (:n args) (-> args :operation conformed-operation-length))))

(defn- text-preserved?
  "True the text of the result operation of the instrumented function
  stands in `relation` to the text of its `operation` argument.
  "
  [relation {:keys [args ret]}]
  (let [insert-text (some->> args :operation ::delta/insert (apply hash-map) :text)]
    (or (nil? insert-text)
        (relation insert-text (-> ret ::delta/insert val)))))

(s/fdef operation/take'
  :args (s/and (s/cat :n pos-int? :operation ::delta/operation)
               #(not (= :embed (some-> % :operation ::delta/insert key)))
               #(<= (:n %) (-> % :operation conformed-operation-length)))
  :ret ::delta/operation
  :fn (s/and operation-type-preserved?
             attributes-preserved?
             operation-shortenend-to-n?
             #(text-preserved? string/starts-with? %)))

(s/fdef operation/take
  :args (s/cat :n int? :operation ::delta/operation)
  :ret (s/nilable ::delta/operation)
  :fn (s/or :nil (s/and #(-> % :args :n pos? not)
                        #(-> % :ret nil?))
            :non-nil (s/and #(-> % :args :n pos?)
                            operation-type-preserved?
                            attributes-preserved?
                            operation-shortenend-to-n?
                            #(text-preserved? string/starts-with? %))))

(defn- length-shortened-by-n?
  "True if the instrumented function shortened its `operation` argument
  by `n` characters (`n` is also an argument).
  "
  [{:keys [args ret]}]
  (= (- (-> args :operation conformed-operation-length) (-> args :n (max 0)))
     (-> ret conformed-operation-length)))

(s/fdef operation/drop'
  :args (s/and (s/cat :n nat-int? :operation ::delta/operation)
               #(not (= :embed (some-> % :operation ::delta/insert key)))
               #(< (:n %) (-> % :operation conformed-operation-length dec)))
  :ret ::delta/operation
  :fn (s/and operation-type-preserved?
             attributes-preserved?
             length-shortened-by-n?
             #(text-preserved? string/ends-with? %)))

(s/fdef operation/drop
  :args (s/cat :n int? :operation ::delta/operation)
  :ret (s/nilable ::delta/operation)
  :fn (s/or :nil (s/and #(>= (-> % :args :n) (-> % :args :operation conformed-operation-length))
                        #(-> % :ret nil?))
            :non-nil (s/and #(< (-> % :args :n) (-> % :args :operation conformed-operation-length))
                            operation-type-preserved?
                            attributes-preserved?
                            length-shortened-by-n?
                            #(text-preserved? string/ends-with? %))))

(defn- has-type
  ""
  [type]
  #(type %))

(s/fdef operation/delete
  :args (s/cat :n int?)
  :ret (s/nilable (s/and ::delta/operation (has-type ::delta/delete)))
  :fn (s/or :nil (s/and #(-> % :args :n pos? not)
                        #(-> % :ret nil?))
            :non-nil (s/and #(-> % :args :n pos?)
                            #(= (-> % :args :n)
                                (-> % :ret conformed-operation-length)))))

(defn- operation-contents-preserved?
  "True if the instrumented function returned an operation with the same
  contents as the function’s `operation` argument. For delete and retain
  operations, the contents is simply the operation’s length. For insert
  operations, the contents is the text or embed.
  "
  [{:keys [args ret]}]
  (let [type (operation/type ret)]
    (= (type ret)
       (-> args :operation type))))

(defn- attributes-set?
  "True if the instrumented function returned an operation with the same
  attributes as the function’s `attributes` argument.
  "
  [{:keys [args ret]}]
  (let [arg-attrs (-> args :attributes)
        ret-attrs (-> ret ::delta/attributes)]
    (or (and (empty? arg-attrs) (empty? ret-attrs))
        (= arg-attrs ret-attrs))))

(s/fdef operation/with-attributes
  :args (s/cat :operation ::delta/operation :attributes (s/nilable attributes))
  :ret ::delta/operation
  :fn (s/and operation-type-preserved?
             operation-contents-preserved?
             attributes-set?))

(s/fdef operation/insert
  :args (s/cat :val (s/alt :text string? :embed embed)
               :attributes (s/? (s/nilable attributes)))
  :ret (s/nilable (s/and ::delta/operation (has-type ::delta/insert)))
  :fn (s/or :nil (s/and #(-> % :args :val val (= ""))
                        #(-> % :ret nil?))
            :non-nil (s/and #(-> % :args :val val (= "") not)
                            attributes-set?
                            #(= (-> % :args :val val)
                                ((-> % :args :val key)
                                 (->> % :ret ::delta/insert (apply hash-map)))))))

(s/fdef operation/retain
  :args (s/cat :n int?
               :attributes (s/? (s/nilable attributes)))
  :ret (s/nilable (s/and ::delta/operation (has-type ::delta/retain)))
  :fn (s/or :nil (s/and #(-> % :args :n pos? not)
                        #(-> % :ret nil?))
            :non-nil (s/and #(-> % :args :n pos?)
                            attributes-set?
                            #(= (-> % :args :n)
                                (-> % :ret conformed-operation-length)))))

(defn- nil-keys
  "Given a map `m`, return a sequence of those keys that are bound to a
  `nil` value."
  [m]
  (into [] (comp (filter (comp nil? val)) (map key)) m))

(defn- dissoc-all
  "Returns a copy of `map` with the `keys` removed."
  [map keys]
  (apply dissoc map keys))

(defn- apply-attribute-diff
  "Given a map of `attributes` and a `diff`, apply the `diff` to the
  `attributes` and return the result.
  "
  [attributes diff]
  (-> attributes
      (merge diff)
      (dissoc-all (nil-keys diff))))

(s/fdef operation/attribute-diff
  :args (s/cat :attributes (s/nilable (s/and attributes #(every? (comp some? val) %)))
               :other-attributes (s/nilable (s/and attributes #(every? (comp some? val) %))))
  :ret attributes
  :fn (s/and #(every? (partial contains? (-> % :args :attributes)) (-> % :ret nil-keys))
             #(= (-> % :args :other-attributes (or {}))
                 (-> % :args :attributes (apply-attribute-diff (:ret %))))))
