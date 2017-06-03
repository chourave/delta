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

(ns plumula.delta.lazy
  (:require [clojure.core :as core]
            [clojure.string :as string]
            [plumula.delta :as delta]
            [plumula.delta.operation :as operation]
            [plumula.delta.util :as util])
  (:refer-clojure :exclude [concat comp drop take]))

(defn push
  "Returns a collection representing `delta` with `new-op` logically appended to
  the end, and that satisfies the delta’s invariant.

  No attempt is made to handle invalid `new-op`s gracefully, for instance
  operations whose length is 0. On the other hand, the `delete`, `insert` and
  `retain` functions perform that kind of checks, making them safer options.

  Examples
  (delta/push {::delta/insert \"y\"} [{::delta/insert \"x\"}])
  ; returns [{::delta/insert \"xy\"}]
  "
  [new-op delta]
  (letfn [(merge-using [key merge ops]
            (let [merged-value (apply merge (map key ops))
                  attr (::delta/attributes new-op)]
              (-> {key merged-value}
                  (util/assoc-if-not-empty ::delta/attributes attr)
                  vector)))

          (merge [tail-ops]
            (cond
              (apply not= (map ::delta/attributes tail-ops))
              tail-ops

              (every? #(string? (::delta/insert %)) tail-ops)
              (merge-using ::delta/insert str tail-ops)

              (every? #(contains? % ::delta/retain) tail-ops)
              (merge-using ::delta/retain + tail-ops)

              :else
              tail-ops))

          (push' [delta]
            (let [op (first delta)
                  tail (next delta)]
              (if tail
                (if-not (and (::delta/insert new-op)
                             (::delta/delete (first tail))
                             (not (next delta)))
                  (cons op (push' tail))
                  ; Since it does not matter if we insert before or after
                  ; deleting at the same index, always prefer to insert first
                  (conj (merge [op new-op]) (first tail)))
                (let [tail-ops [op new-op]]
                  (cond
                    (every? ::delta/delete tail-ops)
                    (merge-using ::delta/delete + tail-ops)

                    op
                    (merge tail-ops)

                    :else
                    [new-op])))))]
    (if new-op
      (lazy-seq (push' delta))
      delta)))

(def delete
  "Returns a collection representing `delta` with a ‘delete `lenght` characters’
  operation logically appended to its end.
  If no `delta` is provided, an empty delta is used as a starting point.

  Examples
  (->> (delta/delete 3) (delta/delete 4)
  ; returns [{::delta/delete 7}]
  "
  (delta/make-delete push))

(def ^{:arglists '([text attributes? delta?] [embed attributes? delta?])} insert
  "Returns a collection representing `delta` with an ‘insert `text`’ or ‘insert
  `embed`’ operation logically appended to its end.
  If no `delta` is provided, an empty delta is used as a starting point.
  You can optionally provide a map of `attributes` for the operation.

  Examples
  (->> (delta/insert \"a\" {:bold true}) (delta/insert \"bc\" {:bold true}))
  ; returns [{::delta/insert \"abc\" ::delta/attributes {:bold true}}]
  "
  (delta/make-insert push))

(def ^{:arglists '([length attributes? delta?])} retain
  "Returns a collection representing `delta` with a ‘retain `length` characters’
  operation logically appended to its end.
  If no `delta` is provided, an empty delta is used as a starting point.
  You can optionally provide a map of `attributes` for the operation.

  Examples
  (->> (delta/retain 3) (delta/retain 4 {:bold true}))
  ; returns [{::delta/retain 3} {::delta/retain 4, ::delta/attributes {:bold true}}]
  "
  (delta/make-retain push))

(defn concat
  "Returns a lazy sequence representing the concatenation of the `delta`s
  supplied as arguments, and that satisfies the delta’s invariant.

  Examples
  (delta/concat (delta/retain 3) (delta/retain 4))
  ; returns [{::delta/retain 7}]
  "
  {:arglists '([delta*])}
  ([] delta/no-delta)
  ([delta] delta)
  ([delta & deltas]
   (let [heads (map first deltas)
         tails (map next deltas)
         joined (mapcat #(if %2 (push %2 %1) %1) (conj tails delta) heads)]
     (if-let [end (last tails)]
       (core/concat joined end)
       joined))))

(defn- split-lines
  ""
  [separator terminator]
  (fn [{text ::delta/insert :as op}]
    (if-not (string? text)
      [op]
      (let [needs-trailer (re-find terminator text)]
        (cond-> text
                needs-trailer (str ".")
                :always (string/split separator)
                :always (->> (interpose ::delta/newline)
                             (remove #{""}))
                needs-trailer drop-last
                :always (->> (map (partial assoc op ::delta/insert))))))))

(defn- build-lines [xf]
  ""
  (let [line (volatile! delta/no-delta)]
    (letfn [(xf-line! [result attributes]
              (let [finished @line]
                (vreset! line delta/no-delta)
                (-> {}
                    (util/assoc-if-not-empty ::delta/line finished)
                    (util/assoc-if-not-empty ::delta/attributes attributes)
                    (->> (xf result)))))
            (flush! [result]
              (-> result (xf-line! nil) unreduced))
            (reducer
              ([] (xf))
              ([result]
               (cond-> result
                       (seq @line) flush!
                       :always xf))
              ([result {text ::delta/insert attributes ::delta/attributes :as op}]
               (if (util/keyword-identical? text ::delta/newline)
                 (xf-line! result attributes)
                 (do
                   (vswap! line #(conj % op))
                   result))))]
      reducer)))

(defn lines
  ""
  ([delta]
   (lines "\n" delta))
  ([newline delta]
   (sequence
     (core/comp
       (take-while ::delta/insert)
       (mapcat (split-lines (re-pattern newline) (re-pattern (str newline "$"))))
       build-lines)
     delta)))
