; Challenge 1 Part 1
(defn eligibility [state corgi_count] (and (pos? corgi_count) (contains? #{"IL", "WA", "NY", "CO"} state) ) )

; Challenge 1 Part 2
(defn sgp [state corgi_count policy_count]
    (if (contains? #{"IL", "WA", "NY", "CO"} state)
        (cond (>= corgi_count 7) :platinum
              (and (>= corgi_count 3) (>= policy_count 1)) :platinum
              (>= corgi_count 3) :gold
              (>= corgi_count 1) :silver
        )
        false
    )
)

; Challenge 1 Part 3
(defn accept-map [{:keys [state corgi-count policy-count]}]
    (if (contains? #{"IL", "WA", "NY", "CO"} state)
        (cond (>= corgi-count 7) :platinum
              (and (>= corgi-count 3) (>= policy-count 1)) :platinum
              (>= corgi-count 3) :gold
              (>= corgi-count 1) :silver
        )
        false
    )
)

; Challenge 1 Part 4
(defn cross-reference [{:keys [name state corgi-count policy-count]} Megacorp-data]
    (if (contains? #{"IL", "WA", "NY", "CO"} state)
        (let [Megacorp-policy-count (count (Megacorp-data name))]
            (cond (>= corgi-count 7) :platinum
                  (and (>= corgi-count 3) (>= (+ policy-count Megacorp-policy-count) 1)) :platinum
                  (>= corgi-count 3) :gold
                  (>= corgi-count 1) :silver
            )
        )
        false
    )
)

; Challenge 2 Part 1
(defn eligibility-check [{:keys [state corgi-count]}] (and (pos? corgi-count) (contains? #{"IL", "WA", "NY", "CO"} state) ) )
(defn read-file []
    (let [dataframe-str (-> (slurp "corgi-cover-applications.csv")
                            (clojure.string/split #"\n")
                            (->> (map #(clojure.string/split % #",") ) )
                        )
          body (for [row (rest dataframe-str)] (conj (subvec row 0 2) (Integer/parseInt (row 2)) (Integer/parseInt (row 3)) ) )
          header [:name :state :corgi-count :policy-count] 
         ]
        (for [row body] (zipmap header row) )
    )
)
(map eligibility-check (read-file))

; Challenge 2 Part 2
(defn create-files []
    (let [application-data (read-file)
          names-of-applicants (for [application application-data] (:name application))
          eligibility (map eligibility-check application-data)
         ]
        (with-open [f-eligible   (clojure.java.io/writer   "eligible-corgi-cover-applications.csv")
                    f-ineligible (clojure.java.io/writer "ineligible-corgi-cover-applications.csv")
                   ]
            (dotimes [i (count application-data)] 
                (if (nth eligibility i) (.write f-eligible   (str (nth names-of-applicants i) \newline))
                                        (.write f-ineligible (str (nth names-of-applicants i) \newline))
                )
            )
        )
    )
)
(create-files)


; 6.6 Exercises
(defn fibonacci [n]
  (->>  (iterate (fn [[a b]] [b (+ a b)]) [0 1]) 
        (take n) 
        (last)
        (first)
  )
)
(defn factorial [n]
  (->>  (iterate (fn [[k fac-k]] [(inc k) (* fac-k (inc k))]) [1 1] )
        (take n)
        (last)
        (last)
  )
)
