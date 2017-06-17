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

(ns plumula.delta.util
  (:require [clojure.core :as core])
  (:refer-clojure :exclude [keyword-identical?]))

#?(:clj  (set! *warn-on-reflection* true)
   :cljs (set! *warn-on-infer* true))

(defn assoc-unless
  "If `v` is `p?`, returns `x` with `k` `dissoc`iated from it.
   Otherwise, returns `x` with `k` `assoc`iated to `v`.

   Examples
   (assoc-unless empty? {::retain 1 ::attributes {:bold true}} ::attributes {})
   ; returns {::retain 1}

   (assoc-unless empty? {::retain 1} ::attributes {:bold true})
   ; returns {::retain 1 ::atributes {:bold true}}
   "
  [p? x k v]
  (if (p? v)
    (dissoc x k)
    (assoc x k v)))

(def keyword-identical? #?(:clj identical? :cljs core/keyword-identical?))
