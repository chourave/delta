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
            [plumula.delta :as delta]
            [plumula.delta.operation :as operation]))

(s/def ::delta/text string?)
(s/def ::delta/embed (s/map-of keyword? any? :kind? #(= count 1)))

(s/def ::delta/insert (s/or ::delta/text ::delta/embed))
(s/def ::delta/delete pos?)
(s/def ::delta/retain pos?)

(s/def ::delta/attributes (s/map-of keyword? any?))

(defmulti operation-spec operation/type)

(defmethod operation-spec ::delta/insert [_]
  (s/keys :req [::delta/insert] :opt [::delta/attributes]))

(defmethod operation-spec ::delta/delete [_]
  (s/keys :req [::delta/delete]))

(defmethod operation-spec ::delta/retain [_]
  (s/keys :req [::delta/retain] :opt [::delta/attributes]))

(s/def ::delta/operation (s/multi-spec operation-spec identity))

(s/def ::delta/delta (s/coll-of ::delta/operation))

(s/def ::delta/line ::delta/delta)
