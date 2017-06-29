(ns plumula.delta.slow.operation-test
    (:require [plumula.delta.spec]
              [plumula.delta.spec.operation]
              [plumula.delta.operation :as operation]
              [plumula.mimolette.alpha :refer [defspec-test]]))

(defspec-test test-op-specs
  `[operation/length
    operation/type
    operation/take'
    operation/take
    operation/drop'
    operation/drop
    operation/delete
    operation/with-attributes
    operation/insert
    operation/retain
    operation/attribute-diff]
  {:opts {:num-tests 10}})
