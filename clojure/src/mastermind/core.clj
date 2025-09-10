(ns mastermind.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn generate-code [] (->> (range 0 9) (vec) (shuffle) (take 4)))

(defn user-input []
  (println "Enter a 4 digit code:")
  (let [code (->> (read-line) (map #(Character/digit %1 10)) (filter (comp not neg?)))]
    (if (and (= 4 (count code)) (= (dedupe (sort code)) (sort code)))
      code
      nil)))
