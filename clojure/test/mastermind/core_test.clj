(ns mastermind.core-test
  (:require [clojure.test :refer :all]
            [mastermind.core :refer :all]))

(deftest a-test
  (testing "Nah I'm good"
    (is (= 0 0))))

(deftest generate-code-test
  (testing "generate code generates a random value of 4 digits"
    (let [code (generate-code)]
      (is (= 4 (count code)))
      (is (= (dedupe (sort code)) (sort code))))))

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
