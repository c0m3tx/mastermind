(ns mastermind.core
  (:gen-class)
  (:use [clojure.set :as set :only (intersection)]))

(defn generate-code [] (->> (range 0 9) (vec) (shuffle) (take 4) (vec)))

(defn valid? [code] (= 4 (count (set code))))

(defn user-input []
  (print "Enter a 4 digit code: ")
  (flush)
  (let [code (->> (read-line) (map #(Character/digit %1 10)) (filter (comp not neg?)))]
    (if (valid? code) code nil)))

(defn xes [code guess] (count (filter identity (map == code guess))))
(defn oes [code guess] (- (count (set/intersection (set code) (set guess))) (xes code guess)))

(defn output-result [xs os] (println "Result:" (apply str (concat (repeat xs "X") (repeat os "O")))))

(defn play-loop [secret attempts]
  (print "You have" attempts "attempts left. ")
  (let [guess (user-input) xs (xes secret guess) os (oes secret guess)]
    (cond 
      (nil? guess) (do (println "Invalid input, enter 4 distinct digits.") (recur secret attempts)) 
      (= 4 xs) (println "You won!")
      (= 1 attempts) (println "You lost, the code was" (str secret))
      :else (do 
              (output-result xs os) 
              (recur secret (dec attempts)))
    )))

(defn -main
  [& args]
  (let [secret (generate-code)]
    (println "Welcome to Mastermind!")
    (play-loop secret 10)
  ))

