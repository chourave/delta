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

(ns plumula.delta-test
  (:require [plumula.delta :as delta]
            [plumula.delta.eager :as eager]
            [plumula.delta.lazy :as lazy]
            [clojure.test :refer [deftest is testing run-tests]]))

(defn test-push
  [push]
  (testing "push"
    (testing "into empty"
      (let [delta []
            delta (push {::delta/insert "test"} delta)]
        (is (= 1 (count delta)))))
    (testing "consecutive deletes"
      (let [delta (eager/delete 2)
            delta (push {::delta/delete 3} delta)]
        (is (= 1 (count delta)))
        (is (= {::delta/delete 5} (first delta)))))
    (testing "consecutive text"
      (let [delta (eager/insert "a")
            delta (push {::delta/insert "b"} delta)]
        (is (= 1 (count delta)))
        (is (= {::delta/insert "ab"} (first delta)))))
    (testing "consecutive texts with matching attributes"
      (let [delta (eager/insert "a" {:bold true})
            delta (push {::delta/insert "b" ::delta/attributes {:bold true}} delta)]
        (is (= 1 (count delta)))
        (is (= {::delta/insert "ab" ::delta/attributes {:bold true}} (first delta)))))
    (testing "consecutive retains with matching attributes"
      (let [delta (eager/retain 1 {:bold true})
            delta (push {::delta/retain 3 ::delta/attributes {:bold true}} delta)]
        (is (= 1 (count delta)))
        (is (= {::delta/retain 4 ::delta/attributes {:bold true}} (first delta)))))
    (testing "consecutive texts with mismatched attributes"
      (let [delta (eager/insert "a" {:bold true})
            delta (push {::delta/insert "b"} delta)]
        (is (= 2 (count delta)))))
    (testing "consecutive embeds with matching attributes"
      (let [delta (eager/insert 1 {:alt "description"})
            delta (push {::delta/insert {:url "http://quilljs.com"} ::delta/attributes {:alt "description"}} delta)]
        (is (= 2 (count delta)))))))

(defn test-delete
  [delete]
  (testing "delete"
    (testing "(0)"
      (let [delta (eager/delete 0)]
        (is (= 0 (count delta)))))
    (testing "(positive)"
      (let [delta (eager/delete 1)]
        (is (= 1 (count delta)))
        (is (= {::delta/delete 1} (first delta)))))))

(defn test-insert
  [insert]
  (testing "insert"
    (testing "(empty)"
      (let [delta (eager/insert "")]
        (is (= 0 (count delta)))))
    (testing "(text)"
      (let [delta (eager/insert "test")]
        (is (= 1 (count delta)))
        (is (= {::delta/insert "test"} (first delta)))))
    (testing "(text, nil)"
      (let [delta (eager/insert "test" nil)]
        (is (= 1 (count delta)))
        (is (= {::delta/insert "test"} (first delta)))))
    (testing "(embed)")
    (let [delta (eager/insert 1)]
      (is (= 1 (count delta)))
      (is (= {::delta/insert 1} (first delta))))
    (testing "(embed, attributes)"
      (let [attributes {:url "http://quilljs.com" :alt "Quill"}
            delta (eager/insert 1 attributes)]
        (is (= 1 (count delta)))
        (is (= {::delta/insert 1 ::delta/attributes attributes} (first delta)))))
    (testing "(embed) non-integer"
      (let [embed {:url "http://quilljs.com"}
            attributes {:alt "Quill"}
            delta (eager/insert embed attributes)]
        (is (= 1 (count delta)))
        (is (= {::delta/insert embed ::delta/attributes attributes} (first delta)))))
    (testing "(text, attributes)"
      (let [delta (eager/insert "test" {:bold true})]
        (is (= 1 (count delta)))
        (is (= {::delta/insert "test" ::delta/attributes {:bold true}} (first delta)))))
    (testing "(text) after delete"
      (let [delta (->> (eager/delete 1) (eager/insert "a"))
            expected (->> (eager/insert "a") (eager/delete 1))]
        (is (= expected delta))))
    (testing "(text) after delete with merge"
      (let [delta (->> (eager/insert "a") (eager/delete 1) (eager/insert "b"))
            expected (->> (eager/insert "ab") (eager/delete 1))]
        (is (= expected delta))))
    (testing "(text) after delete no merge"
      (let [delta (->> (eager/insert 1) (eager/delete 1) (eager/insert "a"))
            expected (->> (eager/insert 1) (eager/insert "a") (eager/delete 1))]
        (is (= expected delta))))
    (testing "(text, {})"
      (is (= (eager/insert "a") (eager/insert "a" {}))))))

(defn test-retain
  [retain]
  (testing "retain"
    (testing "(0)"
      (is (= 0 (count (eager/retain 0)))))
    (testing "(length)"
      (let [delta (eager/retain 2)]
        (is (= 1 (count delta)))
        (is (= {::delta/retain 2} (first delta)))))
    (testing "(length, nil)"
      (let [delta (eager/retain 2 nil)]
        (is (= 1 (count delta)))
        (is (= {::delta/retain 2} (first delta)))))
    (testing "(length, attributes)"
      (let [delta (eager/retain 1 {:bold true})]
        (is (= 1 (count delta)))
        (is (= {::delta/retain 1 ::delta/attributes {:bold true}} (first delta)))))
    (testing "(length, {)"
      (let [delta (->> (eager/retain 2 {}) (eager/delete 1)) ; Delete prevents chop (??)
            expected (->> (eager/retain 2) (eager/delete 1))]
        (is (= expected delta))))))

(deftest test-building
  (testing "lazy"
    (test-push lazy/push)
    (test-delete lazy/delete)
    (test-insert lazy/insert)
    (test-retain lazy/retain))
  (testing "eager"
    (test-push eager/push)
    (test-delete eager/delete)
    (test-insert eager/insert)
    (test-retain eager/retain)))

(defn test-concat
  [concat]
  (testing "empty delta"
    (let [delta (eager/insert "Test")]
      (is (= delta (concat delta)))))
  (testing "unmergeable"
    (let [delta (eager/insert "Test")
          expected (->> (eager/insert "Test") (eager/insert "!" {:bold true}))]
      (is (= expected (concat delta (eager/insert "!" {:bold true}))))))
  (testing "mergeable"
    (let [delta (eager/insert "Test" {:bold true})
          expected (->> (eager/insert "Test!" {:bold true}) (eager/insert "\n"))]
      (is (= expected (concat delta (->> (eager/insert "!" {:bold true}) (eager/insert "\n")))))))
  (testing "other arities"
    (testing "no arg"
      (is (empty? (concat))))
    (testing "1 arg"
      (let [delta (eager/insert "Test")]
        (is (= delta (concat delta)))))
    (testing "3 args, unmergeable"
      (let [d1 (eager/insert "Test")
            d2 (eager/insert "!" {:bold true})
            d3 (eager/insert "\n")
            expected (->> (eager/insert "Test") (eager/insert "!" {:bold true}) (eager/insert "\n"))]
        (is (= expected (concat d1 d2 d3)))))))

(deftest test-helpers
  (testing "concat"
    (testing "lazy"
      (test-concat lazy/concat))
    (testing "eager"
      (test-concat eager/concat)))

  (testing "chop"
    (testing "retain"
      (let [delta (->> (eager/insert "Test") (eager/retain 4))
            expected (eager/insert "Test")]
        (is (= expected (eager/chop delta)))))
    (testing "insert"
      (let [delta (eager/insert "Test")]
        (is (= delta (eager/chop delta)))))
    (testing "formatted retain"
      (let [delta (->> (eager/insert "Test") (eager/retain 4 {:bold true}))]
        (is (= delta (eager/chop delta))))))

  (testing "split-lines"
    (let [split-at-newlines (@#'lazy/split-lines #"\n" #"\n$")]
      (testing "no newline"
        (is (= [{::delta/insert "bla"}] (split-at-newlines {::delta/insert "bla"}))))
      (testing "no newline, keep attributes"
        (is (= [{::delta/insert "bla" ::delta/attributes {:bold true}}] (split-at-newlines {::delta/insert "bla" ::delta/attributes {:bold true}}))))
      (testing "one newline"
        (let [expected [{::delta/insert "bla"} {::delta/insert ::delta/newline} {::delta/insert "gab"}]]
          (is (= expected (split-at-newlines {::delta/insert "bla\ngab"})))))
      (testing "one newline, keep attributes"
        (let [expected [{::delta/insert "bla" ::delta/attributes {:bold true}}
                        {::delta/insert ::delta/newline ::delta/attributes {:bold true}}
                        {::delta/insert "gab" ::delta/attributes {:bold true}}]]
          (is (= expected (split-at-newlines {::delta/insert "bla\ngab" ::delta/attributes {:bold true}})))))
      (testing "two newlines"
        (let [expected [{::delta/insert "a"} {::delta/insert ::delta/newline} {::delta/insert "b"} {::delta/insert ::delta/newline} {::delta/insert "c"}]]
          (is (= expected (split-at-newlines {::delta/insert "a\nb\nc"})))))
      (testing "trailing newline"
        (let [expected [{::delta/insert "a"} {::delta/insert ::delta/newline}]]
          (is (= expected (split-at-newlines {::delta/insert "a\n"})))))
      (testing "double trailing newline"
        (let [expected [{::delta/insert "a"} {::delta/insert ::delta/newline} {::delta/insert ::delta/newline}]]
          (is (= expected (split-at-newlines {::delta/insert "a\n\n"})))))
      (testing "leading newlines"
        (let [expected [{::delta/insert ::delta/newline} {::delta/insert "a"}]]
          (is (= expected (split-at-newlines {::delta/insert "\na"})))))
      (testing "consecutive newlines"
        (let [expected [{::delta/insert "a"} {::delta/insert ::delta/newline} {::delta/insert ::delta/newline} {::delta/insert "b"}]]
          (is (= expected (split-at-newlines {::delta/insert "a\n\nb"})))))
      (testing "newline on its own"
        (let [expected [{::delta/insert ::delta/newline}]]
          (is (= expected (split-at-newlines {::delta/insert ::delta/newline})))))
      (testing "embed"
        (is (= [{::delta/insert 1}] (split-at-newlines {::delta/insert 1}))))))

  (testing "build-lines"
    (let [build-lines @#'lazy/build-lines]
      (testing "empty"
        (is (= [] (sequence build-lines []))))
      (testing "no newline"
        (let [delta [{::delta/insert "ab"}]
              expected [{::delta/line delta}]]
          (is (= expected (sequence build-lines delta)))))
      (testing "one newline"
        (let [delta [{::delta/insert "ab"} {::delta/insert ::delta/newline} {::delta/insert "cd"}]
              expected [{::delta/line [{::delta/insert "ab"}]} {::delta/line [{::delta/insert "cd"}]}]]
          (is (= expected (sequence build-lines delta)))))
      (testing "two newlines"
        (let [delta [{::delta/insert "ab"} {::delta/insert ::delta/newline} {::delta/insert ::delta/newline} {::delta/insert "cd"}]
              expected [{::delta/line [{::delta/insert "ab"}]} {} {::delta/line [{::delta/insert "cd"}]}]]
          (is (= expected (sequence build-lines delta)))))))

  (testing "lines"
    (testing "standard case"
      (let [delta (->> (eager/insert "Hello\n\n")
                       (eager/insert "World" {:bold true})
                       (eager/insert {:image "octocat.png"})
                       (eager/insert "\n" {:align "right"})
                       (eager/insert "!"))
            expected [{::delta/line (eager/insert "Hello")}
                      {}
                      {::delta/line       (->> (eager/insert "World" {:bold true})
                                               (eager/insert {:image "octocat.png"}))
                       ::delta/attributes {:align "right"}}
                      {::delta/line (eager/insert "!")}]]
        (is (= expected (lazy/lines delta)))))
    (testing "trailing newline"
      (let [delta (eager/insert "Hello\nWorld!\n")
            expected [{::delta/line [{::delta/insert "Hello"}]}
                      {::delta/line [{::delta/insert "World!"}]}]]
        (is (= expected (lazy/lines delta)))))
    (testing "non-document"
      (let [delta (->> (eager/retain 1) (eager/delete 2))]
        (is (empty? (lazy/lines delta)))))

    (testing "lenght"
      (testing "document"
        (let [delta (->> (eager/insert "AB" {:bold true}) (eager/insert 1))]
          (is (= 3 (eager/length delta)))))
      (testing "mixed"
        (let [delta (->> (eager/insert "AB" {:bold true})
                         (eager/insert 1)
                         (eager/retain 2 {:bold nil})
                         (eager/delete 1))]
          (is (= 6 (eager/length delta)))))))

  (testing "take and drop"
    (testing "whole op"
      (let [delta (->> (eager/retain 2) (eager/insert "a"))]
        (is (= (eager/retain 2) (eager/take 2 delta)))
        (is (= (eager/insert "a") (eager/drop 2 delta)))))
    (testing "split text"
      (let [delta (->> (eager/insert "0123456789") (eager/insert "a" {:bold true}))
            expected-tail (->> (eager/insert "23456789") (eager/insert "a" {:bold true}))]
        (is (= (eager/insert "01") (eager/take 2 delta)))
        (is (= expected-tail (eager/drop 2 delta)))))
    (testing "split op"
      (let [delta (->> (eager/retain 18) (eager/insert "a" {:bold true}))
            expected-tail (->> (eager/retain 16) (eager/insert "a" {:bold true}))]
        (is (= (eager/retain 2) (eager/take 2 delta)))
        (is (= expected-tail (eager/drop 2 delta)))))
    (testing "multiple ops"
      (let [delta (->> (eager/retain 2) (eager/insert "abcd" {:bold true}) (eager/delete 12))
            expected-head (->> (eager/retain 2) (eager/insert "abcd" {:bold true}) (eager/delete 2))]
        (is (= expected-head (eager/take 8 delta)))
        (is (= (eager/delete 10) (eager/drop 8 delta)))))
    (testing "everything"
      (let [delta (->> (eager/insert "0123"))]
        (is (= delta (eager/take 4 delta)))
        (is (empty? (eager/drop 4 delta)))))
    (testing "overkill"
      (let [delta (->> (eager/insert "0123"))]
        (is (= delta (eager/take 18 delta)))
        (is (empty? (eager/drop 18 delta))))))

  (testing "slice"
    (testing "start"
      (let [slice (->> (eager/retain 2) (eager/insert "a") (eager/slice 2))
            expected (->> (eager/insert "a"))]
        (is (= expected slice))))
    (testing "start and end chop"
      (let [slice (->> (eager/insert "0123456789") (eager/slice 2 7))
            expected (->> (eager/insert "23456"))]
        (is (= expected slice))))
    (testing "start and end multiple chop"
      (let [slice (->> (eager/insert "0123" {:bold true}) (eager/insert "4567") (eager/slice 3 5))
            expected (->> (eager/insert "3" {:bold true}) (eager/insert "4"))]
        (is (= expected slice))))
    (testing "start and end"
      (let [slice (->> (eager/retain 2) (eager/insert "A" {:bold true}) (eager/insert "B") (eager/slice 2 3))
            expected (eager/insert "A" {:bold true})]
        (is (= expected slice))))
    (testing "no param"
      (let [delta (->> (eager/retain 2) (eager/insert "A" {:bold true}) (eager/insert "B"))
            slice (eager/slice delta)]
        (is (= delta slice))))
    (testing "split ops"
      (let [slice (->> (eager/insert "AB" {:bold true}) (eager/insert "C") (eager/slice 1 2))
            expected (->> (eager/insert "B" {:bold true}))]
        (is (= expected slice))))
    (testing "split ops multiple times"
      (let [slice (->> (eager/insert "ABC" {:bold true}) (eager/insert "D") (eager/slice 1 2))
            expected (->> (eager/insert "B" {:bold true}))]
        (is (= expected slice))))))

(deftest test-rebase
  (testing "rebase"
    (testing "insert + insert"
      (let [a (eager/insert "A")
            b (eager/insert "B")
            expected (->> (eager/retain 1) (eager/insert "B"))]
        (is (= expected (eager/rebase a b :base-insert-first true)))
        (is (= b (eager/rebase a b)))))
    (testing "insert + retain"
      (let [a (eager/insert "A")
            b (eager/retain 1 {:bold true :color "red"})
            expected (->> (eager/retain 1) (eager/retain 1 {:bold true :color "red"}))]
        (is (= expected (eager/rebase a b)))))
    (testing "insert + delete"
      (let [a (eager/insert "A")
            b (eager/delete 1)
            expected (->> (eager/retain 1) (eager/delete 1))]
        (is (= expected (eager/rebase a b)))))
    (testing "delete + insert"
      (let [a (eager/delete 1)
            b (eager/insert "B")]
        (is (= b (eager/rebase a b)))))
    (testing "delete + retain"
      (let [a (eager/delete 1)
            b (eager/retain 1 {:bold true :color "red"})]
        (is (= [] (eager/rebase a b)))))
    (testing "delete + delete"
      (let [a (eager/delete 1)
            b (eager/delete 1)]
        (is (= [] (eager/rebase a b)))))
    (testing "retain + insert"
      (let [a (eager/retain 1 {:color "blue"})
            b (eager/insert "B")]
        (is (= b (eager/rebase a b)))))
    (testing "retain + retain"
      (let [a (eager/retain 1 {:color "blue"})
            b (eager/retain 1 {:bold true :color "red"})
            expected (eager/retain 1 {:bold true})]
        (is (= expected (eager/rebase a b :prefer-base-attributes true)))
        (is (= [] (eager/rebase b a :prefer-base-attributes true)))))
    (testing "retain + retain without priority"
      (let [a (eager/retain 1 {:color "blue"})
            b (eager/retain 1 {:bold true :color "red"})]
        (is (= b (eager/rebase a b)))
        (is (= a (eager/rebase b a)))))
    (testing "retain + delete"
      (let [a (eager/retain 1 {:color "blue"})
            b (eager/delete 1)]
        (is (= b (eager/rebase a b)))))
    (testing "alternating edits"
      (let [a (->> (eager/retain 2) (eager/insert "si") (eager/delete 5))
            b (->> (eager/retain 1) (eager/insert "e") (eager/delete 5) (eager/retain 1) (eager/insert "ow"))
            expected1 (->> (eager/retain 1) (eager/insert "e") (eager/delete 1) (eager/retain 2) (eager/insert "ow"))
            expected2 (->> (eager/retain 2) (eager/insert "si") (eager/delete 1))]
        (is (= expected1 (eager/rebase a b)))
        (is (= expected2 (eager/rebase b a)))))
    (testing "conflicting appends"
      (let [a (->> (eager/retain 3) (eager/insert "aa"))
            b (->> (eager/retain 3) (eager/insert "bb"))
            expected (->> (eager/retain 5) (eager/insert "bb"))]
        (is (= expected (eager/rebase a b :base-insert-first true)))
        (is (= a (eager/rebase b a)))))
    (testing "prepend + append"
      (let [a (eager/insert "aa")
            b (->> (eager/retain 3) (eager/insert "bb"))
            expected (->> (eager/retain 5) (eager/insert "bb"))]
        (is (= expected (eager/rebase a b)))
        (is (= a (eager/rebase b a)))))
    (testing "trailing deletes with differing lenghts"
      (let [a (->> (eager/retain 2) (eager/delete 1))
            b (eager/delete 3)]
        (is (= (eager/delete 2) (eager/rebase a b)))
        (is (= [] (eager/rebase b a)))))))

(deftest test-rebase-position
  (testing "rebase-position"
    (testing "insert before position"
      (let [delta (eager/insert "A")]
        (is (= 3 (eager/rebase-position delta 2)))))
    (testing "insert after position"
      (let [delta (->> (eager/retain 2) (eager/insert "A"))]
        (is (= 1 (eager/rebase-position delta 1)))))
    (testing "insert at position"
      (let [delta (->> (eager/retain 2) (eager/insert "A"))]
        (is (= 2 (eager/rebase-position delta 2 :insert-after-position true)))
        (is (= 3 (eager/rebase-position delta 2)))))
    (testing "delete before position"
      (let [delta (eager/delete 2)]
        (is (= 2 (eager/rebase-position delta 4)))))
    (testing "delete after position"
      (let [delta (->> (eager/retain 4) (eager/delete 2))]
        (is (= 2 (eager/rebase-position delta 2)))))
    (testing "delete across position"
      (let [delta (->> (eager/retain 1) (eager/delete 4))]
        (is (= 1 (eager/rebase-position delta 2)))))
    (testing "insert and delete before position"
      (let [delta (->> (eager/retain 2) (eager/insert "A") (eager/delete 2))]
        (is (= 3 (eager/rebase-position delta 4)))))
    (testing "delete before and across position"
      (let [delta (->> (eager/delete 1) (eager/retain 1) (eager/delete 4))]
        (is (= 1 (eager/rebase-position delta 4)))))))

(deftest test-compose
  (testing "insert + insert"
    (is (= (eager/insert "BA") (eager/comp (eager/insert "A") (eager/insert "B")))))
  (testing "insert + retain"
    (let [b (eager/retain 1 {:bold true :color "red" :font nil})
          expected (eager/insert "A" {:bold true :color "red"})]
      (is (= expected (eager/comp (eager/insert "A") b)))))
  (testing "insert + delete"
    (is (= [] (eager/comp (eager/insert "A") (eager/delete 1)))))
  (testing "delete + insert"
    (let [expected (->> (eager/insert "B") (eager/delete 1))]
      (is (= expected (eager/comp (eager/delete 1) (eager/insert "B"))))))
  (testing "delete + retain"
    (let [b (eager/retain 1 {:bold true :color "red"})
          expected (->> (eager/delete 1) (eager/retain 1 {:bold true :color "red"}))]
      (is (= expected (eager/comp (eager/delete 1) b)))))
  (testing "delete + delete"
    (is (= (eager/delete 2) (eager/comp (eager/delete 1) (eager/delete 1)))))
  (testing "retain + insert"
    (let [a (eager/retain 1 {:color "blue"})
          expected (->> (eager/insert "B") (eager/retain 1 {:color "blue"}))]
      (is (= expected (eager/comp a (eager/insert "B"))))))
  (testing "retain + retain"
    (let [a (eager/retain 1 {:color "blue"})
          b (eager/retain 1 {:bold true :color "red" :font nil})]
      (is (= b (eager/comp a b)))))
  (testing "retain + delete"
    (let [a (eager/retain 1 {:color "blue"})]
      (is (= (eager/delete 1) (eager/comp a (eager/delete 1))))))
  (testing "insert in middle of text"
    (let [b (->> (eager/retain 3) (eager/insert "X"))]
      (is (= (eager/insert "HelXlo") (eager/comp (eager/insert "Hello") b)))))
  (testing "insert and delete ordering"
    (let [insert-first (->> (eager/retain 3) (eager/insert "X") (eager/delete 1))
          delete-first (->> (eager/retain 3) (eager/delete 1) (eager/insert "X"))]
      (is (= (eager/insert "HelXo") (eager/comp (eager/insert "Hello") insert-first)))
      (is (= (eager/insert "HelXo") (eager/comp (eager/insert "Hello") delete-first)))))
  (testing "insert embed"
    (let [a (eager/insert 1 {:src "http://quilljs.com/image.png"})
          b (eager/retain 1 {:alt "logo"})
          expected (eager/insert 1 {:src "http://quilljs.com/image.png" :alt "logo"})]))
  (testing "delete entire text"
    (let [a (->> (eager/retain 4) (eager/insert "Hello"))]
      (is (= (eager/delete 4) (eager/comp a (eager/delete 9))))))
  (testing "retain more than length of text"
    (let [a (eager/insert "Hello")]
      (is (= a (eager/comp a (eager/retain 10))))))
  (testing "retain empty embed"
    (let [a (eager/insert 1)]
      (is (= a (eager/comp a (eager/retain 1))))))
  (testing "remove all attributes"
    (let [a (eager/insert "A" {:bold true})
          b (eager/retain 1 {:bold nil})]
      (is (= (eager/insert "A") (eager/comp a b)))))
  (testing "remove all embed attributes"
    (let [a (eager/insert 2 {:bold true})
          b (eager/retain 1 {:bold nil})]
      (is (= (eager/insert 2) (eager/comp a b))))))

(deftest test-diff
  (let [character-set @#'eager/character-set
        embed-set @#'eager/embed-set]
    (testing "character-set"
      (testing "one string"
        (is (= #{\a \b \c} (character-set (eager/insert "abc")))))
      (testing "many strings"
        (is (= #{\a \b \c \d \e \f}
               (character-set (->> (eager/insert "abc")
                                   (eager/insert "d" {:bold true})
                                   (eager/insert "ef"))))))
      (testing "one embed"
        (is (= #{} (character-set (eager/insert {:img "gna"})))))
      (testing "mixing embeds and text"
        (is (= #{\c \o \n \h \f \l \u \b}
               (character-set (->> (eager/insert "conch")
                                   (eager/insert {:img "gna"})
                                   (eager/insert "flub")))))))
    (testing "embed-set"
      (testing "one embed"
        (is (= #{{:img "gna"}} (embed-set (eager/insert {:img "gna"})))))
      (testing "many embed"
        (is (= #{{:img "gna"} {:img "yo"} {:img "blu"}}
               (embed-set (->> (eager/insert {:img "gna"})
                               (eager/insert {:img "yo"} {:bold true})
                               (eager/insert {:img "blu"}))))))
      (testing "one string"
        (is (= #{} (embed-set (eager/insert "abc")))))
      (testing "mixing embeds and text"
        (is (= #{{:img "gna"}}
               (embed-set (->> (eager/insert "conch")
                               (eager/insert {:img "gna"})
                               (eager/insert "flub")))))))
    (testing "embed-mapping"
      (let [embed-mapping @#'eager/embed-mapping
            doc (->> (eager/insert "flub")
                     (eager/insert {:img "ya"})
                     (eager/insert {:video "ya"})
                     (eager/insert "grobnik furkulam\n"))
            chars (character-set doc)
            embeds (embed-set doc)
            mapping (embed-mapping doc)]
        (testing "assigns a distinct character to every embed"
          (is (= embeds (set (keys mapping))))
          (is (= (count embeds) (count (-> mapping vals set))))
          (is (every? string? (vals mapping)))
          (is (every? #(= 1 (count %)) (vals mapping))))
        (testing "does not assign the NUL and newline characters"
          (is (not (some #{\u0000 \newline} (vals mapping)))))
        (testing "assigns a character to every embed"
          (is (every? some? (map mapping #{{:img "ya"} {:video "ya"}})))))))
  (testing "stringify"
      (let [char->int @#'eager/char->int
            stringify @#'eager/stringify
            mapping {{:img "bla"} "_"}]
        (testing "one string"
          (is (= "abc" (stringify mapping (eager/insert "abc")))))
        (testing "many strings"
          (is (= "abcdef"
                 (stringify mapping (->> (eager/insert "abc")
                                         (eager/insert "d" {:bold true})
                                         (eager/insert "ef"))))))
        (testing "one embed"
          (is (= "_" (stringify mapping (eager/insert {:img "bla"})))))
        (testing "mixing embeds and text"
          (is (= "conch_flub"
                 (stringify mapping (->> (eager/insert "conch")
                                         (eager/insert {:img "bla"})
                                         (eager/insert "flub")))))))))
