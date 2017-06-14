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

(s/def ::delta/text (s/and string? #(-> % count pos?)))
(s/def ::delta/embed (s/map-of keyword? any? :kind #(= (count %) 1)))

(s/def ::delta/insert (s/or :text ::delta/text :embed ::delta/embed))
(s/def ::delta/delete pos-int?)
(s/def ::delta/retain pos-int?)

(s/def ::delta/attributes (s/map-of keyword? any? :kind seq))

(defmulti operation-spec #(when (map? %) (operation/type %)))

(defmethod operation-spec ::delta/insert [_]
  (s/keys :req [::delta/insert] :opt [::delta/attributes]))

(defmethod operation-spec ::delta/delete [_]
  (s/keys :req [::delta/delete]))

(defmethod operation-spec ::delta/retain [_]
  (s/keys :req [::delta/retain] :opt [::delta/attributes]))

(s/def ::delta/operation (s/multi-spec operation-spec (fn [o _] o)))

(defn unconform-operation
  ""
  [conformed-operation]
  (if (::delta/insert conformed-operation)
    (update conformed-operation ::delta/insert val)
    conformed-operation))

(s/def ::delta/delta (s/coll-of ::delta/operation))

(s/fdef operation/length
  :args (s/cat :operation ::delta/operation)
  :ret pos-int?)

(s/fdef operation/type
  :args (s/cat :operation ::delta/operation)
  :ret #{::delta/insert ::delta/delete ::delta/retain})

(defn- operation-type-preserved?
  ""
  [{:keys [args ret]}]
  (= (-> ret operation/type)
     (-> args :operation operation/type)))

(defn- attributes-preserved?
  ""
  [{:keys [args ret]}]
  (= (-> ret ::delta/attributes)
     (-> args :operation ::delta/attributes)))

(defn- length-clipped-to-n?
  ""
  [{:keys [args ret]}]
  (= (-> ret unconform-operation operation/length)
     (min (:n args) (-> args :operation unconform-operation operation/length))))

(defn- text-preserved?
  ""
  [relation {:keys [args ret]}]
  (let [insert-text (some->> args :operation ::delta/insert (apply hash-map) :text)]
    (or (nil? insert-text)
        (relation insert-text (-> ret ::delta/insert val)))))

(s/fdef operation/take'
  :args (s/and (s/cat :n pos-int? :operation ::delta/operation)
               #(not (= :embed (some-> % :operation ::delta/insert key)))
               #(<= (:n %) (-> % :operation unconform-operation operation/length)))
  :ret ::delta/operation
  :fn (s/and operation-type-preserved?
             attributes-preserved?
             length-clipped-to-n?
             #(text-preserved? string/starts-with? %)))

(s/fdef operation/take
  :args (s/cat :n int? :operation ::delta/operation)
  :ret (s/nilable ::delta/operation)
  :fn (s/or :nil (s/and #(-> % :args :n pos? not)
                        #(-> % :ret nil?))
            :non-nil (s/and #(-> % :args :n pos?)
                            operation-type-preserved?
                            attributes-preserved?
                            length-clipped-to-n?
                            #(text-preserved? string/starts-with? %))))

(defn- length-shortened-by-n?
  ""
  [{:keys [args ret]}]
  (= (- (-> args :operation unconform-operation operation/length) (-> args :n (max 0)))
     (-> ret unconform-operation operation/length)))

(s/fdef operation/drop'
  :args (s/and (s/cat :n nat-int? :operation ::delta/operation)
               #(not (= :embed (some-> % :operation ::delta/insert key)))
               #(< (:n %) (-> % :operation unconform-operation operation/length dec)))
  :ret ::delta/operation
  :fn (s/and operation-type-preserved?
             attributes-preserved?
             length-shortened-by-n?
             #(text-preserved? string/ends-with? %)))

(s/fdef operation/drop
  :args (s/cat :n int? :operation ::delta/operation)
  :ret (s/nilable ::delta/operation)
  :fn (s/or :nil (s/and #(>= (-> % :args :n) (-> % :args :operation unconform-operation operation/length))
                        #(-> % :ret nil?))
            :non-nil (s/and #(< (-> % :args :n) (-> % :args :operation unconform-operation operation/length))
                            operation-type-preserved?
                            attributes-preserved?
                            length-shortened-by-n?
                            #(text-preserved? string/ends-with? %))))

(s/fdef operation/delete
  :args (s/cat :n int?)
  :ret (s/nilable (s/and ::delta/operation
                         #(::delta/delete %)))
  :fn (s/or :nil (s/and #(-> % :args :n pos? not)
                        #(-> % :ret nil?))
            :non-nil (s/and #(-> % :args :n pos?)
                            #(= (-> % :args :n)
                                (-> % :ret operation/length)))))

(defn- operation-contents-preserved?
  ""
  [{:keys [args ret]}]
  (let [type (operation/type ret)]
    (= (type ret)
       (-> args :operation type))))

(defn- attributes-set?
  ""
  [{:keys [args ret]}]
  (let [arg-attrs (-> args :attributes)
        ret-attrs (-> ret ::delta/attributes)]
    (or (and (empty? arg-attrs) (empty? ret-attrs))
        (= arg-attrs ret-attrs))))

(s/fdef operation/with-attributes
  :args (s/cat :operation ::delta/operation :attributes (s/nilable (s/map-of keyword? any?)))
  :ret ::delta/operation
  :fn (s/and operation-type-preserved?
             operation-contents-preserved?
             attributes-set?))

(s/fdef operation/insert
  :args (s/cat :val (s/alt :text string? :embed ::delta/embed)
               :attributes (s/? (s/nilable (s/map-of keyword? any?))))
  :ret (s/nilable (s/and ::delta/operation
                         #(::delta/insert %)))
  :fn (s/or :nil (s/and #(-> % :args :val val (= ""))
                        #(-> % :ret nil?))
            :non-nil (s/and #(-> % :args :val val (= "") not)
                            attributes-set?
                            #(= (-> % :args :val val)
                                ((-> % :args :val key)
                                  (->> % :ret ::delta/insert (apply hash-map)))))))
