(ns mastermind.core-test
  (:require [clojure.test :refer :all]
            [mastermind.core :refer :all]))

(deftest a-test
  (testing "Nah I'm good"
    (is (= 0 0))))

(deftest valid-test
  (testing "valid verifies that the code has the right length and no dups"
    (is (valid? [1 2 3 4]))
    (is (not (valid? [1 2 3])))
    (is (not (valid? [1 1 3 4])))))

(deftest generate-code-test
  (testing "generate code generates a random value of 4 digits"
    (let [code (generate-code)]
      (is (valid? code)))))

(deftest user-input-test
  (testing "user input accepts valid input"
    (with-redefs [read-line (fn [] "1234"), println (fn [& _] nil)]
      (let [code (user-input)]
        (is (= code [1 2 3 4]))))
    (with-redefs [read-line (fn [] "1123"), println (fn [& _] nil)]
      (let [code (user-input)]
        (is (nil? code))))
    (with-redefs [read-line (fn [] "123"), println (fn [& _] nil)]
      (let [code (user-input)]
        (is (nil? code))))
    (with-redefs [read-line (fn [] "12a4"), println (fn [& _] nil)]
      (let [code (user-input)]
        (is (nil? code))))
    (with-redefs [read-line (fn [] "1a345"), println (fn [& _] nil)]
      (let [code (user-input)]
        (is (= code [1 3 4 5]))))))

(deftest xes-test
  (testing "xes are calculated correctly"
    (is (= 4 (xes [1 2 3 4] [1 2 3 4])))
    (is (= 3 (xes [1 2 3 4] [1 2 3 5])))
    (is (= 2 (xes [1 2 3 4] [2 1 3 4])))
    (is (= 1 (xes [1 2 3 4] [1 5 6 7])))
    (is (= 0 (xes [1 2 3 4] [5 6 7 8])))
    ))

(deftest oes-test
  (testing "oes are calculated correctly"
    (is (= 0 (oes [1 2 3 4] [5 2 6 7])))
    (is (= 1 (oes [1 2 3 4] [2 5 6 7])))
    (is (= 2 (oes [1 2 3 4] [2 1 3 4])))
    (is (= 3 (oes [1 2 3 4] [2 3 1 4])))
    (is (= 4 (oes [1 2 3 4] [2 1 4 3])))
    ))
