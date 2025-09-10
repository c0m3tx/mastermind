(ns mastermind.core-test
  (:require [clojure.test :refer :all]
            [mastermind.core :refer :all]))

(deftest a-test
  (testing "Nah I'm good"
    (is (= 0 0))))

(deftest generate-code-test
  (testing "generate code generates a random value of 4 digits"
    (let [code (generate-code)]
      (println code)
      (is (= 4 (count code)))
      (is (every? (fn [c] (= 1 (count (filter (partial == c) code)))) code)))))
