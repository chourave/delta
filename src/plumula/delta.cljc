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
  (:require [plumula.delta.operation :as operation]))

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

(defn make-delete
  [push]
  (fn delete
    ([length]
     (delete length no-delta))
    ([length delta]
     (push (operation/delete length) delta))))

(defn make-insert
  [push]
  (fn insert
    ([text-or-embed]
     (insert text-or-embed nil no-delta))
    ([text-or-embed delta-or-attributes]
     (if (map? delta-or-attributes)
       (insert text-or-embed delta-or-attributes no-delta)
       (insert text-or-embed nil delta-or-attributes)))
    ([text-or-embed attributes delta]
     (push (operation/insert text-or-embed attributes) delta))))

(defn make-retain
  [push]
  (fn retain
    ([length]
     (retain length nil no-delta))
    ([length delta-or-attributes]
     (if (map? delta-or-attributes)
       (retain length delta-or-attributes no-delta)
       (retain length nil delta-or-attributes)))
    ([length attributes delta]
     (push (operation/retain length attributes) delta))))
