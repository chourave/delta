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

(ns plumula.delta.operation-test
  (:require [plumula.delta :as delta]
            [plumula.delta.operation :as operation]
            [clojure.test :refer [deftest is testing run-tests]]))

(deftest test-operations
  (testing "operation-length"
    (testing "delete"
      (let [op (operation/delete 5)]
        (is (= 5 (operation/length op)))))
    (testing "retain"
      (let [op (operation/retain 2)]
        (is (= 2 (operation/length op)))))
    (testing "text"
      (let [op (operation/insert "text")]
        (is (= 4 (operation/length op)))))
    (testing "embed"
      (let [op (operation/insert 2)]
        (is (= 1 (operation/length op))))))

  (testing "take-operation and drop-operation"
    (testing "delete"
      (let [op (operation/delete 5)]
        (is (= (operation/delete 3) (operation/take 3 op)))
        (is (= (operation/delete 2) (operation/drop 3 op)))))
    (testing "delete > lenght"
      (let [op (operation/delete 5)]
        (is (= op (operation/take 8 op)))
        (is (nil? (operation/drop 8 op)))))
    (testing "retain"
      (let [op (operation/retain 7)]
        (is (= (operation/retain 2) (operation/take 2 op)))
        (is (= (operation/retain 5) (operation/drop 2 op)))))
    (testing "retain > lenght"
      (let [op (operation/retain 4)]
        (is (= op (operation/take 6 op)))
        (is (nil? (operation/drop 6 op)))))
    (testing "retain attributes"
      (let [op (operation/retain 6 {:bold true})]
        (is (= (operation/retain 5 {:bold true}) (operation/take 5 op)))
        (is (= (operation/retain 1 {:bold true}) (operation/drop 5 op)))))
    (testing "text"
      (let [op (operation/insert "text")]
        (is (= (operation/insert "tex") (operation/take 3 op)))
        (is (= (operation/insert "t") (operation/drop 3 op)))))
    (testing "text > length"
      (let [op (operation/insert "text")]
        (is (= op (operation/take 5 op)))
        (is (nil? (operation/drop 5 op)))))
    (testing "text attributes"
      (let [op (operation/insert "text" {:bold true})]
        (is (= (operation/insert "tex" {:bold true}) (operation/take 3 op)))
        (is (= (operation/insert "t" {:bold true}) (operation/drop 3 op)))))
    (testing "embed"
      (let [op (operation/insert 42)]
        (is (= op (operation/take 1 op)))
        (is (nil? (operation/drop 1 op))))))

  (testing "operation-type"
    (let [op (operation/insert "abc" {:bold true})]
      (is (= ::delta/insert (operation/type op))))
    (let [op (operation/retain 3 {:bold true})]
      (is (= ::delta/retain (operation/type op))))
    (let [op (operation/delete 5)]
      (is (= ::delta/delete (operation/type op))))))
