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

(ns plumula.delta.operation
  (:require [plumula.delta.util :as util])
  (:refer-clojure :exclude [drop take type]))

(def ^:private infinity
  "Positive infinity.
  "
  #?(:clj  Double/POSITIVE_INFINITY
     :cljs js/Infinity))

(def nop
  "An operation that does nothing when applied to a text, and that is
  invariant under `operation/drop`.
  Most of the time you really want `no-delta`.

  Examples
  (operation/drop 15 operation/nop)
  ; returns operation/drop

  (delta/comp a-delta [operation/nop])
  ; returns a-delta
  "
  {:plumula.delta/retain infinity})

(defn length
  "The number of characters affected by `op`.
  See also `length`.

  Examples
  (operation/length {operation/delete 3))
  ; returns 3

  (operation/length (operation/insert \"hi\"))
  ; returns 2
  "
  [op]
  (or
    (:plumula.delta/delete op)
    (:plumula.delta/retain op)
    (if (string? (:plumula.delta/insert op))
      (count (:plumula.delta/insert op))
      1)))

(defn type
  "Returns one of the :plumula.delta/delete, :plumula.delta/insert or
  :plumula.delta/retain keywords depending on which type `op` is.

  Examples
  (operation/type (operation/delete 3))
  ; returns ::delta/delete
  "
  [op]
  (->> (keys op)
       (some #{:plumula.delta/delete :plumula.delta/insert :plumula.delta/retain})))

(defn take'
  "Returns an operation with the n first characters of op.
   No bounds checking, and does not work for embeds!
   The safer alternative is `take`."
  [n op]
  (if-let [key (some #{:plumula.delta/delete :plumula.delta/retain} (keys op))]
    (assoc op key n)
    (update op :plumula.delta/insert #(subs % 0 n))))

(defn take
  "Returns an operation with the n first characters of op."
  [n op]
  (if (>= n (length op))
    op
    (when n
      (take' n op))))

(defn drop'
  "Returns an operation without the n first characters of op.
   No bounds checking, does not work for embeds!
   The safer alternative is `drop`."
  [n op]
  (if-let [key (some #{:plumula.delta/delete :plumula.delta/retain} (keys op))]
    (update op key #(- % n))
    (update op :plumula.delta/insert #(subs % n))))

(defn drop
  "Returns an operation without the n first characters of op."
  [n op]
  (if-not n
    op
    (when (< n (length op))
      (drop' n op))))


(defn delete
  "Returns a map representing a ‘delete `lenght` characters’ operation.

  Examples
  (->> (delta/delete 3)
  ; returns {::delta/delete 7}
  "
  ([length]
   (when (pos? length)
     {:plumula.delta/delete length})))

(defn- with-attributes
  ""
  [op attributes]
  (util/assoc-if-not-empty op :plumula.delta/attributes attributes))

(defn insert
  "Returns a map representing an ‘insert `text`’ or ‘insert `embed`’ operation.

  Examples
  (delta/insert \"abc\" {:bold true})
  ; returns {::delta/insert \"abc\" ::delta/attributes {:bold true}}
  "
  {:arglists '([text attributes?] [embed attributes?])}
  ([text-or-embed]
   (insert text-or-embed nil))
  ([text-or-embed attributes]
   (when (not= "" text-or-embed)
     (-> {:plumula.delta/insert text-or-embed}
         (with-attributes attributes)))))

(defn retain
  "Returns a map representing a ‘retain `length` characters’ operation.

  Examples
  (delta/retain 3)
  ; returns {::delta/retain 3}
  "
  {:arglists '([length attributes?])}
  ([length]
   (retain length nil))
  ([length attributes]
   (when (pos? length)
     (-> {:plumula.delta/retain length}
         (with-attributes attributes)))))
