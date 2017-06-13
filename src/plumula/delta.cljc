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

(ns plumula.delta
  (:require [clojure.core :as core]
            [clojure.string :as string]
            [plumula.delta.operation :as operation]
            [plumula.delta.util :as util]
            [plumula.diff :as diff])
  (:refer-clojure :exclude [concat comp drop take]))

(def no-delta
  "A delta that does nothing when applied to a text, and that is
  invariant under `drop`.

  Examples
  (delta/drop 15 delta/no-delta)
  ; returns a collection that is `=` to delta/no-delta

  (delta/comp delta delta/no-delta)
  ; returns delta
  "
  [])

(defn- without-last
  ""
  [delta]
  (subvec delta 0 (dec (count delta))))

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
  (letfn [(merge-using [key merge delta]
            (let [merged-value (apply merge (map key [(last delta) new-op]))
                  attr (::attributes new-op)
                  new-op(util/assoc-unless empty? {key merged-value} ::attributes attr)]
              (-> delta without-last (conj new-op))))

          (push-into [new-op delta]
            (let [tail-ops [(last delta) new-op]]
              (cond
                (apply not= (map ::attributes tail-ops))
                (conj delta new-op)

                (every? #(string? (::insert %)) tail-ops)
                (merge-using ::insert str delta)

                (every? ::retain tail-ops)
                (merge-using ::retain + delta)

                :else
                (conj delta new-op))))]

    (let [delta (vec delta)
          last-op (last delta)]
      (cond
        (not new-op)
        delta

        (every? ::delete [last-op new-op])
        (merge-using ::delete + delta)

        ; Since it does not matter if we insert before or after deleting at the
        ; same index, always prefer to insert first)
        (and (::delete last-op) (::insert new-op))
        (-> delta without-last (->> (push-into new-op)) (conj last-op))

        :else
        (push-into new-op delta)))))

(defn delete
  "Returns a collection representing `delta` with a ‘delete `lenght` characters’
  operation logically appended to its end.
  If no `delta` is provided, an empty delta is used as a starting point.

  Examples
  (->> (delta/delete 3) (delta/delete 4)
  ; returns [{::delta/delete 7}]
  "
  ([length]
    (delete length no-delta))
  ([length delta]
    (push (operation/delete length) delta)))

(defn insert
  "Returns a collection representing `delta` with an ‘insert `text`’ or ‘insert
  `embed`’ operation logically appended to its end.
  If no `delta` is provided, an empty delta is used as a starting point.
  You can optionally provide a map of `attributes` for the operation.

  Examples
  (->> (delta/insert \"a\" {:bold true}) (delta/insert \"bc\" {:bold true}))
  ; returns [{::delta/insert \"abc\" ::delta/attributes {:bold true}}]
  "
  {:arglists '([text attributes? delta?] [embed attributes? delta?])}
  ([text-or-embed]
   (insert text-or-embed nil no-delta))
  ([text-or-embed delta-or-attributes]
   (if (map? delta-or-attributes)
     (insert text-or-embed delta-or-attributes no-delta)
     (insert text-or-embed nil delta-or-attributes)))
  ([text-or-embed attributes delta]
   (push (operation/insert text-or-embed attributes) delta)))

(defn retain
  "Returns a collection representing `delta` with a ‘retain `length` characters’
  operation logically appended to its end.
  If no `delta` is provided, an empty delta is used as a starting point.
  You can optionally provide a map of `attributes` for the operation.

  Examples
  (->> (delta/retain 3) (delta/retain 4 {:bold true}))
  ; returns [{::delta/retain 3} {::delta/retain 4, ::delta/attributes {:bold true}}]
  "
  {:arglists '([length attributes? delta?])}
  ([length]
   (retain length nil no-delta))
  ([length delta-or-attributes]
   (if (map? delta-or-attributes)
     (retain length delta-or-attributes no-delta)
     (retain length nil delta-or-attributes)))
  ([length attributes delta]
   (push (operation/retain length attributes) delta)))


(defn length
  "The number of characters affected by `delta`.

  Examples
  (delta/length (delta/retain 4))
  ; returns 4

  (delta/length (delta/insert \"sup\"))
  ; returns 3
  "
  [delta]
  (apply + (map operation/length delta)))

(defn chop
  "Returns a collection representing the `delta` with the last operation removed
  if it is an attribute-less retain. These can be safely removed (and the delta
  minimality invariant requires them to be removed) because every delta is
  defined to carry an implicit `nop` as its last element.

  Examples
  (delta/chop (->> (delta/insert \"bla\") (delta/retain 2)))
  ; result is ({::delta/insert \"bla\"})
  "
  [delta]
  (let [delta (vec delta)
        last-op (peek delta)]
    (if (and (::retain last-op) (not (::attributes last-op)))
      (without-last delta)
      delta)))

(defn take
  "Returns a delta with the n first characters of delta"
  [n delta]
  (loop [n n
         delta delta
         result no-delta]
    (if (empty? delta)
      result
      (let [[op & tail] delta
            m (- n (operation/length op))]
        (cond
          (pos? m) (recur m tail (conj result op))
          (zero? m) (conj result op)
          :else (conj result (operation/take' n op)))))))

(defn drop
  "Returns a delta without the n first characters of delta"
  [n delta]
  (when (seq delta)
    (let [[op & tail] delta
          m (- n (operation/length op))]
      (cond
        (pos? m) (recur m tail)
        (zero? m) tail
        :else (cons (operation/drop' n op) tail)))))

(defn slice
  ""
  ([delta] delta)
  ([start delta] (drop start delta))
  ([start end delta] (->> delta (drop start) (take (- end start)))))

(defn concat
  "Returns a collection representing the concatenation of the `delta`s
  supplied as arguments, and that satisfies the delta’s invariant.

  Examples
  (delta/concat (delta/retain 3) (delta/retain 4))
  ; returns [{::delta/retain 7}]
  "
  {:arglists '([delta*])}
  ([] no-delta)
  ([& deltas]
   (reduce (fn [result [op & delta]]
             (cond-> (push op result)
                     (seq delta) (into delta)))
           deltas)))

(defn rebase
  ""
  [base delta & {:keys [base-insert-first prefer-base-attributes]}]
  (let [rebase-attrs (if-not prefer-base-attributes
                       (fn [base-op op] (::attributes op))
                       (fn [base-op op]
                         (apply dissoc (::attributes op) (keys (::attributes base-op)))))]
    (loop [[base-op :as base] base
           [op :as delta] delta
           result no-delta]
      (cond
        (empty? delta)
        (chop result)

        (empty? base)
        (concat result delta)

        (and (::insert base-op) (or base-insert-first (not (::insert op))))
        (recur (next base) delta (retain (operation/length base-op) result))

        (::insert op)
        (recur base (next delta) (push op result))

        :else
        (let [l (apply min (map operation/length [base-op op]))]
          (recur
            (drop l base)
            (drop l delta)
            (cond
              (::delete base-op) result
              (::delete op) (delete l result)
              :else (retain l (rebase-attrs base-op op) result))))))))

(defn rebase-position
  ""
  [base position & {:keys [insert-after-position]}]
  (loop [[op & tail :as base] base
         position position
         offset 0]
    (cond
      (or (empty? base) (> offset position))
      position

      (::delete op)
      (recur tail (max offset (- position (::delete op))) offset)

      (and (::insert op) (or (< offset position) (not insert-after-position)))
      (let [l (operation/length op)]
        (recur tail (+ position l) (+ offset l)))

      :else
      (recur tail position (+ offset (operation/length op))))))

(defn comp
  ""
  [second-delta first-delta]
  (letfn [(merge-non-nil [attr2 attr1]
            (reduce (fn [x [k v]] (util/assoc-unless nil? x k v)) attr2 attr1))

          (compose-into [op2 op1 l result]
            (if (::delete op1)
              (if (::insert op2)
                result
                ; else op1 is a ::retain
                (delete l result))
              ; else op2 is a ::retain
              (let [merge-fn (if (::insert op2) merge-non-nil merge)
                    attrs (merge-fn (::attributes op2) (::attributes op1))]
                (as-> (operation/take l op2) op
                    (util/assoc-unless empty? op ::attributes attrs)
                    (push op result)))))

          (compose' [[op2 :as delta2] [op1 :as delta1] result]
            (cond
              (empty? delta1)
              (concat result delta2)

              (empty? delta2)
              (concat result delta1)

              (::insert op1)
              (recur delta2 (next delta1) (push op1 result))

              (::delete op2)
              (recur (next delta2) delta1 (push op2 result))

              :else
              (let [l (apply min (map operation/length [op2 op1]))
                    result (compose-into op2 op1 l result)]
                (recur (drop l delta2) (drop l delta1) result))))]

    (-> (compose' first-delta second-delta no-delta)
        chop)))

(defn- character-set
  ""
  [inserts]
  (->> inserts
       (eduction (filter string?))
       (apply core/concat)
       set))

(defn- embed-set
  ""
  [inserts]
  (->> inserts
       (eduction (remove string?))
       set))

(defn- available-characters
  ""
  [used-characters]
  (->> (range)
       (drop 32)
       (eduction
         (core/comp
           (map char)
           (remove used-characters)
           (map str)))))

(defn- embed-mapping
  ""
  [inserts]
  (let [chars (character-set inserts)
        embeds (embed-set inserts)
        embed-chars (available-characters chars)]
    (zipmap embeds embed-chars)))

(defn- stringify
  ""
  [mapping inserts]
  (->> inserts
       (eduction (mapcat #(if (string? %) % (mapping %))))
       (apply str)))

(defn- diff-equal
  ""
  [length delta other result]
  (if (zero? length)
    [delta other result]
    (let [match-length (min length
                            (-> delta first operation/length)
                            (-> other first operation/length))]
      (recur (- length match-length)
             (drop match-length delta)
             (drop match-length other)
             (retain match-length
                     (operation/attribute-diff
                       (-> delta first ::attributes)
                       (-> other first ::attributes))
                     result)))))

(defn diff
  ""
  [delta other & opts]
  (cond
    (not-every? ::insert (core/concat delta other))
    (throw (ex-info "diff called on non-document" {}))

    (= delta other)
    no-delta

    :else
    (let [[delta-inserts other-inserts] (map #(map ::insert %) [delta other])
          mapping (embed-mapping (core/concat delta-inserts other-inserts))
          [delta-str other-str] (map #(stringify mapping %) [delta-inserts other-inserts])]
      (loop [[d & d-rest] (apply diff/diff delta-str other-str opts)
             delta delta
             other other
             result []]
        (let [op-length (-> d ::diff/text count)]
          (case (::diff/operation d)
            ::diff/insert
            (recur d-rest
                   delta
                   (drop op-length other)
                   (concat result (take op-length other)))

            ::diff/delete
            (recur d-rest
                   (drop op-length delta)
                   other
                   (delete op-length result))

            ::diff/equal
            (let [[delta other result] (diff-equal op-length delta other result)]
              (recur d-rest delta other result))

            nil
            (chop result)))))))

(defn- split-lines
  ""
  [separator terminator]
  (fn [{text ::insert :as op}]
    (if-not (string? text)
      [op]
      (let [needs-trailer (re-find terminator text)]
        (cond-> text
                needs-trailer (str ".")
                :always (string/split separator)
                :always (->> (interpose ::newline)
                             (remove #{""}))
                needs-trailer drop-last
                :always (->> (map (partial assoc op ::insert))))))))

(defn- build-lines [xf]
  ""
  (let [line (volatile! no-delta)]
    (letfn [(xf-line! [result attributes]
              (let [finished @line]
                (vreset! line no-delta)
                (as-> {} op
                    (util/assoc-unless empty? op ::line finished)
                    (util/assoc-unless empty? op ::attributes attributes)
                    (xf result op))))
            (flush! [result]
              (-> result (xf-line! nil) unreduced))
            (reducer
              ([] (xf))
              ([result]
               (cond-> result
                       (seq @line) flush!
                       :always xf))
              ([result {text ::insert attributes ::attributes :as op}]
               (if (util/keyword-identical? text ::newline)
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
       (take-while ::insert)
       (mapcat (split-lines (re-pattern newline) (re-pattern (str newline "$"))))
       build-lines)
     delta)))
