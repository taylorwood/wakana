(ns wakana.util)

(defn itermap
  "Returns a map-like transducer that calls f with the result of its previous
   invocation as the first arg. Calls f with just the element for the first
   element."
  [f]
  (fn [rf]
    (let [prev (volatile! ::void)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (let [p @prev
               value (if (= ::void p)
                       (f input)
                       (f p input))]
           (vreset! prev value)
           (rf result value)))
        ([result input & inputs]
         (let [p @prev
               value (if (= ::void p)
                       (apply f input inputs)
                       (apply f p input inputs))]
           (vreset! prev value)
           (rf result value)))))))

(comment
  (sequence
   (itermap (fn
              ([x] (prn 'first) (str x))
              ([prev x] (prn prev x) (str x))))
   (range 10))

  (sequence
   (itermap (fn
              ([x y] (prn 'first) (+ x y))
              ([prev x y] (prn prev) (+ prev x y))))
   (range 10)
   (range 10 20)))
