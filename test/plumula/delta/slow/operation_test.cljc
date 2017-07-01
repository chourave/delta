(ns plumula.delta.slow.operation-test
    (:require [plumula.delta.spec]
              [plumula.delta.spec.operation]
              [plumula.delta.operation :as operation]
              [plumula.mimolette.alpha :refer [defspec-test]]))

(defspec-test test-lenght `operation/length {:opts {:num-tests 10}})
(defspec-test test-type `operation/type {:opts {:num-tests 10}})
(defspec-test test-take' `operation/take' {:opts {:num-tests 10}})
(defspec-test test-take `operation/take {:opts {:num-tests 10}})
(defspec-test test-drop' `operation/drop' {:opts {:num-tests 10}})
(defspec-test test-drop `operation/drop {:opts {:num-tests 10}})
(defspec-test test-delete `operation/delete {:opts {:num-tests 10}})
(defspec-test test-with-attributes `operation/with-attributes {:opts {:num-tests 10}})
(defspec-test test-insert `operation/insert {:opts {:num-tests 10}})
(defspec-test test-retain `operation/retain {:opts {:num-tests 10}})
(defspec-test test-attribute-diff `operation/attribute-diff {:opts {:num-tests 10}})
