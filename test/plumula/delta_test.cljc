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
            [clojure.test :refer [deftest is testing run-tests]]))

(deftest test-building
  (testing "delta"
    (testing "push"
      (testing "into empty"
        (let [delta []
              delta (delta/push {::delta/insert "test"} delta)]
          (is (= 1 (count delta)))))
      (testing "consecutive deletes"
        (let [delta (delta/delete 2)
              delta (delta/push {::delta/delete 3} delta)]
          (is (= 1 (count delta)))
          (is (= {::delta/delete 5} (first delta)))))
      (testing "consecutive text"
        (let [delta (delta/insert "a")
              delta (delta/push {::delta/insert "b"} delta)]
          (is (= 1 (count delta)))
          (is (= {::delta/insert "ab"} (first delta)))))
      (testing "consecutive texts with matching attributes"
        (let [delta (delta/insert "a" {:bold true})
              delta (delta/push {::delta/insert "b" ::delta/attributes {:bold true}} delta)]
          (is (= 1 (count delta)))
          (is (= {::delta/insert "ab" ::delta/attributes {:bold true}} (first delta)))))
      (testing "consecutive retains with matching attributes"
        (let [delta (delta/retain 1 {:bold true})
              delta (delta/push {::delta/retain 3 ::delta/attributes {:bold true}} delta)]
          (is (= 1 (count delta)))
          (is (= {::delta/retain 4 ::delta/attributes {:bold true}} (first delta)))))
      (testing "consecutive texts with mismatched attributes"
        (let [delta (delta/insert "a" {:bold true})
              delta (delta/push {::delta/insert "b"} delta)]
          (is (= 2 (count delta)))))
      (testing "consecutive embeds with matching attributes"
        (let [delta (delta/insert 1 {:alt "description"})
              delta (delta/push {::delta/insert {:url "http://quilljs.com"} ::delta/attributes {:alt "description"}} delta)]
          (is (= 2 (count delta))))))
    (testing "delete"
      (testing "(0)"
        (let [delta (delta/delete 0)]
          (is (= 0 (count delta)))))
      (testing "(positive)"
        (let [delta (delta/delete 1)]
          (is (= 1 (count delta)))
          (is (= {::delta/delete 1} (first delta))))))
    (testing "insert"
      (testing "(empty)"
        (let [delta (delta/insert "")]
          (is (= 0 (count delta)))))
      (testing "(text)"
        (let [delta (delta/insert "test")]
          (is (= 1 (count delta)))
          (is (= {::delta/insert "test"} (first delta)))))
      (testing "(text, nil)"
        (let [delta (delta/insert "test" nil)]
          (is (= 1 (count delta)))
          (is (= {::delta/insert "test"} (first delta)))))
      (testing "(embed)")
      (let [delta (delta/insert 1)]
        (is (= 1 (count delta)))
        (is (= {::delta/insert 1} (first delta))))
      (testing "(embed, attributes)"
        (let [attributes {:url "http://quilljs.com" :alt "Quill"}
              delta (delta/insert 1 attributes)]
          (is (= 1 (count delta)))
          (is (= {::delta/insert 1 ::delta/attributes attributes} (first delta)))))
      (testing "(embed) non-integer"
        (let [embed {:url "http://quilljs.com"}
              attributes {:alt "Quill"}
              delta (delta/insert embed attributes)]
          (is (= 1 (count delta)))
          (is (= {::delta/insert embed ::delta/attributes attributes} (first delta)))))
      (testing "(text, attributes)"
        (let [delta (delta/insert "test" {:bold true})]
          (is (= 1 (count delta)))
          (is (= {::delta/insert "test" ::delta/attributes {:bold true}} (first delta)))))
      (testing "(text) after delete"
        (let [delta (->> (delta/delete 1) (delta/insert "a"))
              expected (->> (delta/insert "a") (delta/delete 1))]
          (is (= expected delta))))
      (testing "(text) after delete with merge"
        (let [delta (->> (delta/insert "a") (delta/delete 1) (delta/insert "b"))
              expected (->> (delta/insert "ab") (delta/delete 1))]
          (is (= expected delta))))
      (testing "(text) after delete no merge"
        (let [delta (->> (delta/insert 1) (delta/delete 1) (delta/insert "a"))
              expected (->> (delta/insert 1) (delta/insert "a") (delta/delete 1))]
          (is (= expected delta))))
      (testing "(text, {})"
        (is (= (delta/insert "a") (delta/insert "a" {})))))
    (testing "retain"
      (testing "(0)"
        (is (= 0 (count (delta/retain 0)))))
      (testing "(length)"
        (let [delta (delta/retain 2)]
          (is (= 1 (count delta)))
          (is (= {::delta/retain 2} (first delta)))))
      (testing "(length, nil)"
        (let [delta (delta/retain 2 nil)]
          (is (= 1 (count delta)))
          (is (= {::delta/retain 2} (first delta)))))
      (testing "(length, attributes)"
        (let [delta (delta/retain 1 {:bold true})]
          (is (= 1 (count delta)))
          (is (= {::delta/retain 1 ::delta/attributes {:bold true}} (first delta)))))
      (testing "(length, {)"
        (let [delta (->> (delta/retain 2 {}) (delta/delete 1)) ; Delete prevents chop (??)
              expected (->> (delta/retain 2) (delta/delete 1))]
          (is (= expected delta)))))))

(deftest test-helpers
  (testing "concat"
    (testing "empty delta"
      (let [delta (delta/insert "Test")]
        (is (= delta (delta/concat delta)))))
    (testing "unmergeable"
      (let [delta (delta/insert "Test")
            expected (->> (delta/insert "Test") (delta/insert "!" {:bold true}))]
        (is (= expected (delta/concat delta (delta/insert "!" {:bold true}))))))
    (testing "mergeable"
      (let [delta (delta/insert "Test" {:bold true})
            expected (->> (delta/insert "Test!" {:bold true}) (delta/insert "\n"))]
        (is (= expected (delta/concat delta (->> (delta/insert "!" {:bold true}) (delta/insert "\n")))))))
    (testing "other arities"
      (testing "no arg"
        (is (empty? (delta/concat))))
      (testing "1 arg"
        (let [delta (delta/insert "Test")]
          (is (= delta (delta/concat delta)))))
      (testing "3 args, unmergeable"
        (let [d1 (delta/insert "Test")
              d2 (delta/insert "!" {:bold true})
              d3 (delta/insert "\n")
              expected (->> (delta/insert "Test") (delta/insert "!" {:bold true}) (delta/insert "\n"))]
          (is (= expected (delta/concat d1 d2 d3)))))))

  (testing "regex->str"
    (let [regex->str @#'delta/regex->str]
    (testing "with regex"
      (let [string (regex->str #"\n")]
        (is (string? string))
        (is (= (str #"\n") (str (re-pattern string))))))
    (testing "with string"
      (let [string (regex->str "\\n")]
        (is (string? string))
        (is (= (str #"\n") (str (re-pattern string))))))))

  (testing "chop"
    (testing "retain"
      (let [delta (->> (delta/insert "Test") (delta/retain 4))
            expected (delta/insert "Test")]
        (is (= expected (delta/chop delta)))))
    (testing "insert"
      (let [delta (delta/insert "Test")]
        (is (= delta (delta/chop delta)))))
    (testing "formatted retain"
      (let [delta (->> (delta/insert "Test") (delta/retain 4 {:bold true}))]
        (is (= delta (delta/chop delta))))))

  (testing "split-lines"
    (let [split-at-newlines (@#'delta/split-lines #"\n")]
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
    (let [build-lines @#'delta/build-lines]
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
      (let [delta (->> (delta/insert "Hello\n\n")
                       (delta/insert "World" {:bold true})
                       (delta/insert {:image "octocat.png"})
                       (delta/insert "\n" {:align "right"})
                       (delta/insert "!"))
            expected [{::delta/line (delta/insert "Hello")}
                      {}
                      {::delta/line       (->> (delta/insert "World" {:bold true})
                                               (delta/insert {:image "octocat.png"}))
                       ::delta/attributes {:align "right"}}
                      {::delta/line (delta/insert "!")}]]
        (is (= expected (delta/lines delta)))))
    (testing "trailing newline"
      (let [delta (delta/insert "Hello\nWorld!\n")
            expected [{::delta/line [{::delta/insert "Hello"}]}
                      {::delta/line [{::delta/insert "World!"}]}]]
        (is (= expected (delta/lines delta)))))
    (testing "non-document"
      (let [delta (->> (delta/retain 1) (delta/delete 2))]
        (is (empty? (delta/lines delta)))))

    (testing "lenght"
      (testing "document"
        (let [delta (->> (delta/insert "AB" {:bold true}) (delta/insert 1))]
          (is (= 3 (delta/length delta)))))
      (testing "mixed"
        (let [delta (->> (delta/insert "AB" {:bold true})
                         (delta/insert 1)
                         (delta/retain 2 {:bold nil})
                         (delta/delete 1))]
          (is (= 6 (delta/length delta)))))))

  (testing "take and drop"
    (testing "whole op"
      (let [delta (->> (delta/retain 2) (delta/insert "a"))]
        (is (= (delta/retain 2) (delta/take 2 delta)))
        (is (= (delta/insert "a") (delta/drop 2 delta)))))
    (testing "split text"
      (let [delta (->> (delta/insert "0123456789") (delta/insert "a" {:bold true}))
            expected-tail (->> (delta/insert "23456789") (delta/insert "a" {:bold true}))]
        (is (= (delta/insert "01") (delta/take 2 delta)))
        (is (= expected-tail (delta/drop 2 delta)))))
    (testing "split op"
      (let [delta (->> (delta/retain 18) (delta/insert "a" {:bold true}))
            expected-tail (->> (delta/retain 16) (delta/insert "a" {:bold true}))]
        (is (= (delta/retain 2) (delta/take 2 delta)))
        (is (= expected-tail (delta/drop 2 delta)))))
    (testing "multiple ops"
      (let [delta (->> (delta/retain 2) (delta/insert "abcd" {:bold true}) (delta/delete 12))
            expected-head (->> (delta/retain 2) (delta/insert "abcd" {:bold true}) (delta/delete 2))]
        (is (= expected-head (delta/take 8 delta)))
        (is (= (delta/delete 10) (delta/drop 8 delta)))))
    (testing "everything"
      (let [delta (->> (delta/insert "0123"))]
        (is (= delta (delta/take 4 delta)))
        (is (empty? (delta/drop 4 delta)))))
    (testing "overkill"
      (let [delta (->> (delta/insert "0123"))]
        (is (= delta (delta/take 18 delta)))
        (is (empty? (delta/drop 18 delta))))))

  (testing "slice"
    (testing "start"
      (let [slice (->> (delta/retain 2) (delta/insert "a") (delta/slice 2))
            expected (->> (delta/insert "a"))]
        (is (= expected slice))))
    (testing "start and end chop"
      (let [slice (->> (delta/insert "0123456789") (delta/slice 2 7))
            expected (->> (delta/insert "23456"))]
        (is (= expected slice))))
    (testing "start and end multiple chop"
      (let [slice (->> (delta/insert "0123" {:bold true}) (delta/insert "4567") (delta/slice 3 5))
            expected (->> (delta/insert "3" {:bold true}) (delta/insert "4"))]
        (is (= expected slice))))
    (testing "start and end"
      (let [slice (->> (delta/retain 2) (delta/insert "A" {:bold true}) (delta/insert "B") (delta/slice 2 3))
            expected (delta/insert "A" {:bold true})]
        (is (= expected slice))))
    (testing "no param"
      (let [delta (->> (delta/retain 2) (delta/insert "A" {:bold true}) (delta/insert "B"))
            slice (delta/slice delta)]
        (is (= delta slice))))
    (testing "split ops"
      (let [slice (->> (delta/insert "AB" {:bold true}) (delta/insert "C") (delta/slice 1 2))
            expected (->> (delta/insert "B" {:bold true}))]
        (is (= expected slice))))
    (testing "split ops multiple times"
      (let [slice (->> (delta/insert "ABC" {:bold true}) (delta/insert "D") (delta/slice 1 2))
            expected (->> (delta/insert "B" {:bold true}))]
        (is (= expected slice))))))

(deftest test-rebase
  (testing "rebase"
    (testing "insert + insert"
      (let [a (delta/insert "A")
            b (delta/insert "B")
            expected (->> (delta/retain 1) (delta/insert "B"))]
        (is (= expected (delta/rebase a b :base-insert-first true)))
        (is (= b (delta/rebase a b)))))
    (testing "insert + retain"
      (let [a (delta/insert "A")
            b (delta/retain 1 {:bold true :color "red"})
            expected (->> (delta/retain 1) (delta/retain 1 {:bold true :color "red"}))]
        (is (= expected (delta/rebase a b)))))
    (testing "insert + delete"
      (let [a (delta/insert "A")
            b (delta/delete 1)
            expected (->> (delta/retain 1) (delta/delete 1))]
        (is (= expected (delta/rebase a b)))))
    (testing "delete + insert"
      (let [a (delta/delete 1)
            b (delta/insert "B")]
        (is (= b (delta/rebase a b)))))
    (testing "delete + retain"
      (let [a (delta/delete 1)
            b (delta/retain 1 {:bold true :color "red"})]
        (is (= [] (delta/rebase a b)))))
    (testing "delete + delete"
      (let [a (delta/delete 1)
            b (delta/delete 1)]
        (is (= [] (delta/rebase a b)))))
    (testing "retain + insert"
      (let [a (delta/retain 1 {:color "blue"})
            b (delta/insert "B")]
        (is (= b (delta/rebase a b)))))
    (testing "retain + retain"
      (let [a (delta/retain 1 {:color "blue"})
            b (delta/retain 1 {:bold true :color "red"})
            expected (delta/retain 1 {:bold true})]
        (is (= expected (delta/rebase a b :prefer-base-attributes true)))
        (is (= [] (delta/rebase b a :prefer-base-attributes true)))))
    (testing "retain + retain without priority"
      (let [a (delta/retain 1 {:color "blue"})
            b (delta/retain 1 {:bold true :color "red"})]
        (is (= b (delta/rebase a b)))
        (is (= a (delta/rebase b a)))))
    (testing "retain + delete"
      (let [a (delta/retain 1 {:color "blue"})
            b (delta/delete 1)]
        (is (= b (delta/rebase a b)))))
    (testing "alternating edits"
      (let [a (->> (delta/retain 2) (delta/insert "si") (delta/delete 5))
            b (->> (delta/retain 1) (delta/insert "e") (delta/delete 5) (delta/retain 1) (delta/insert "ow"))
            expected1 (->> (delta/retain 1) (delta/insert "e") (delta/delete 1) (delta/retain 2) (delta/insert "ow"))
            expected2 (->> (delta/retain 2) (delta/insert "si") (delta/delete 1))]
        (is (= expected1 (delta/rebase a b)))
        (is (= expected2 (delta/rebase b a)))))
    (testing "conflicting appends"
      (let [a (->> (delta/retain 3) (delta/insert "aa"))
            b (->> (delta/retain 3) (delta/insert "bb"))
            expected (->> (delta/retain 5) (delta/insert "bb"))]
        (is (= expected (delta/rebase a b :base-insert-first true)))
        (is (= a (delta/rebase b a)))))
    (testing "prepend + append"
      (let [a (delta/insert "aa")
            b (->> (delta/retain 3) (delta/insert "bb"))
            expected (->> (delta/retain 5) (delta/insert "bb"))]
        (is (= expected (delta/rebase a b)))
        (is (= a (delta/rebase b a)))))
    (testing "trailing deletes with differing lenghts"
      (let [a (->> (delta/retain 2) (delta/delete 1))
            b (delta/delete 3)]
        (is (= (delta/delete 2) (delta/rebase a b)))
        (is (= [] (delta/rebase b a)))))))

(deftest test-rebase-position
  (testing "rebase-position"
    (testing "insert before position"
      (let [delta (delta/insert "A")]
        (is (= 3 (delta/rebase-position delta 2)))))
    (testing "insert after position"
      (let [delta (->> (delta/retain 2) (delta/insert "A"))]
        (is (= 1 (delta/rebase-position delta 1)))))
    (testing "insert at position"
      (let [delta (->> (delta/retain 2) (delta/insert "A"))]
        (is (= 2 (delta/rebase-position delta 2 :insert-after-position true)))
        (is (= 3 (delta/rebase-position delta 2)))))
    (testing "delete before position"
      (let [delta (delta/delete 2)]
        (is (= 2 (delta/rebase-position delta 4)))))
    (testing "delete after position"
      (let [delta (->> (delta/retain 4) (delta/delete 2))]
        (is (= 2 (delta/rebase-position delta 2)))))
    (testing "delete across position"
      (let [delta (->> (delta/retain 1) (delta/delete 4))]
        (is (= 1 (delta/rebase-position delta 2)))))
    (testing "insert and delete before position"
      (let [delta (->> (delta/retain 2) (delta/insert "A") (delta/delete 2))]
        (is (= 3 (delta/rebase-position delta 4)))))
    (testing "delete before and across position"
      (let [delta (->> (delta/delete 1) (delta/retain 1) (delta/delete 4))]
        (is (= 1 (delta/rebase-position delta 4)))))))

(deftest test-comp
  (testing "insert + insert"
    (is (= (delta/insert "BA") (delta/comp (delta/insert "B") (delta/insert "A")))))
  (testing "insert + retain"
    (let [b (delta/retain 1 {:bold true :color "red" :font nil})
          expected (delta/insert "A" {:bold true :color "red"})]
      (is (= expected (delta/comp b (delta/insert "A"))))))
  (testing "insert + delete"
    (is (= [] (delta/comp (delta/delete 1) (delta/insert "A")))))
  (testing "delete + insert"
    (let [expected (->> (delta/insert "B") (delta/delete 1))]
      (is (= expected (delta/comp (delta/insert "B") (delta/delete 1))))))
  (testing "delete + retain"
    (let [b (delta/retain 1 {:bold true :color "red"})
          expected (->> (delta/delete 1) (delta/retain 1 {:bold true :color "red"}))]
      (is (= expected (delta/comp b (delta/delete 1))))))
  (testing "delete + delete"
    (is (= (delta/delete 2) (delta/comp (delta/delete 1) (delta/delete 1)))))
  (testing "retain + insert"
    (let [a (delta/retain 1 {:color "blue"})
          expected (->> (delta/insert "B") (delta/retain 1 {:color "blue"}))]
      (is (= expected (delta/comp (delta/insert "B") a)))))
  (testing "retain + retain"
    (let [a (delta/retain 1 {:color "blue"})
          b (delta/retain 1 {:bold true :color "red" :font nil})]
      (is (= b (delta/comp b a)))))
  (testing "retain + delete"
    (let [a (delta/retain 1 {:color "blue"})]
      (is (= (delta/delete 1) (delta/comp (delta/delete 1) a)))))
  (testing "insert in middle of text"
    (let [b (->> (delta/retain 3) (delta/insert "X"))]
      (is (= (delta/insert "HelXlo") (delta/comp b (delta/insert "Hello"))))))
  (testing "insert and delete ordering"
    (let [insert-first (->> (delta/retain 3) (delta/insert "X") (delta/delete 1))
          delete-first (->> (delta/retain 3) (delta/delete 1) (delta/insert "X"))]
      (is (= (delta/insert "HelXo") (delta/comp insert-first (delta/insert "Hello"))))
      (is (= (delta/insert "HelXo") (delta/comp delete-first (delta/insert "Hello"))))))
  (testing "insert embed"
    (let [a (delta/insert 1 {:src "http://quilljs.com/image.png"})
          b (delta/retain 1 {:alt "logo"})
          expected (delta/insert 1 {:src "http://quilljs.com/image.png" :alt "logo"})]))
  (testing "delete entire text"
    (let [a (->> (delta/retain 4) (delta/insert "Hello"))]
      (is (= (delta/delete 4) (delta/comp (delta/delete 9) a)))))
  (testing "retain more than length of text"
    (let [a (delta/insert "Hello")]
      (is (= a (delta/comp (delta/retain 10) a)))))
  (testing "retain empty embed"
    (let [a (delta/insert 1)]
      (is (= a (delta/comp (delta/retain 1) a)))))
  (testing "remove all attributes"
    (let [a (delta/insert "A" {:bold true})
          b (delta/retain 1 {:bold nil})]
      (is (= (delta/insert "A") (delta/comp b a)))))
  (testing "remove all embed attributes"
    (let [a (delta/insert 2 {:bold true})
          b (delta/retain 1 {:bold nil})]
      (is (= (delta/insert 2) (delta/comp b a))))))

(deftest test-diff
  (let [character-set @#'delta/character-set
        embed-set @#'delta/embed-set]
    (testing "character-set"
      (testing "one string"
        (is (= #{\a \b \c} (character-set ["abc"]))))
      (testing "many strings"
        (is (= #{\a \b \c \d \e \f}
               (character-set ["abc" "d" "ef"]))))
      (testing "one embed"
        (is (= #{} (character-set [{:img "gna"}]))))
      (testing "mixing embeds and text"
        (is (= #{\c \o \n \h \f \l \u \b}
               (character-set ["conch" {:img "gna"} "flub"])))))
    (testing "embed-set"
      (testing "one embed"
        (is (= #{{:img "gna"}} (embed-set [{:img "gna"}]))))
      (testing "many embeds"
        (is (= #{{:img "gna"} {:img "yo"} {:img "blu"}}
               (embed-set [{:img "gna"} {:img "yo"} {:img "blu"}]))))
      (testing "one string"
        (is (= #{} (embed-set ["abc"]))))
      (testing "mixing embeds and text"
        (is (= #{{:img "gna"}}
               (embed-set ["conch" {:img "gna"} "flub"])))))
    (testing "embed-mapping"
      (let [embed-mapping @#'delta/embed-mapping
            doc ["flub"
                 {:img "ya"}
                 {:video "ya"}
                 "grobnik furkulam\n"]
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
    (let [stringify @#'delta/stringify
          mapping {{:img "bla"} "_"}]
      (testing "one string"
        (is (= "abc" (stringify mapping ["abc"]))))
      (testing "many strings"
        (is (= "abcdef"
               (stringify mapping ["abc" "d" {:bold true} "ef"]))))
      (testing "one embed"
        (is (= "_" (stringify mapping [{:img "bla"}]))))
      (testing "mixing embeds and text"
        (is (= "conch_flub"
               (stringify mapping ["conch" {:img "bla"} "flub"]))))))
  (testing "diff"
    (testing "insert"
      (is (= (->> (delta/retain 1) (delta/insert "B"))
             (delta/diff (delta/insert "A") (delta/insert "AB")))))
    (testing "delete"
      (is (= (->> (delta/retain 1) (delta/delete 1))
             (delta/diff (delta/insert "AB") (delta/insert "A")))))
    (testing "retain"
      (is (= []
             (delta/diff (delta/insert "A") (delta/insert "A")))))
    (testing "format"
      (is (= (delta/retain 1 {:bold true})
             (delta/diff (delta/insert "A") (delta/insert "A" {:bold true})))))
    (testing "object attributes"
      (is (= (delta/retain 1 {:font {:family "Helvetica" :size "16px"}})
             (delta/diff (delta/insert "A" {:font {:family "Helvetica" :size "15px"}})
                         (delta/insert "A" {:font {:family "Helvetica" :size "16px"}})))))
    (testing "embed integer match"
      (is (empty (delta/diff (delta/insert 1) (delta/insert 1)))))
    (testing "embed integer mismatch"
      (is (= (->> (delta/delete 1) (delta/insert 2))
             (delta/diff (delta/insert 1) (delta/insert 2)))))
    (testing "embed object match"
      (is (empty (delta/diff (delta/insert {:image "http://quilljs.com"})
                             (delta/insert {:image "http://quilljs.com"})))))
    (testing "embed object mismatch"
      (is (= (->> (delta/insert {:image "http://github.com"})
                  (delta/delete 1))
             (delta/diff (delta/insert {:image "http://quilljs.com"})
                         (delta/insert {:image "http://github.com"})))))
    (testing "inconvenient indexes"
      (is (= (->> (delta/insert "Good" {:bold true})
                  (delta/delete 2)
                  (delta/retain 1 {:italic true, :color nil})
                  (delta/delete 3)
                  (delta/insert "og" {:italic true}))
             (delta/diff
               (->> (delta/insert "Bad" {:color "red"})
                    (delta/insert "cat" {:color "blue"}))
               (->> (delta/insert "Good" {:bold true})
                    (delta/insert "dog" {:italic true}))))))
    (testing "same document"
      (let [a (->> (delta/insert "A") (delta/insert "B" {:bold true}))]
        (is (empty? (delta/diff a a)))))
    (testing "non-document"
      (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"diff called on non-document"
                            (delta/diff (delta/insert "Test")
                                        (delta/delete 4)))))))
