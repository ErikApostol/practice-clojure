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
(require '[clojure.java.io :as io])
(require '[clojure.data.csv :as csv])
(def four-states #{"IL", "WA", "NY", "CO"})
(def in-file "corgi-cover-applications.csv")
(def in-header-vec [:name :state :corgi-count :policy-count])
(defn convert-value-to-int [record-map ky] (update record-map ky #(Integer/parseInt %) ) )

(defn eligibility-check [{:keys [state corgi-count]}] 
    (and (pos? corgi-count) (contains? four-states state) ) 
)

(defn read-file []
    (with-open [reader (io/reader in-file)]
        (->>
            (csv/read-csv reader)
            rest
            (mapv zipmap (repeat in-header-vec))
            (mapv #(convert-value-to-int % :corgi-count) )
            (mapv #(convert-value-to-int % :policy-count) )
        )
    )
)
(map eligibility-check (read-file))

; Challenge 2 Part 2
(defn create-files []
    (let [application-list-map (read-file)
          result-list-map (for [application-map application-list-map] 
                               (-> (assoc application-map :eligibility (eligibility-check application-map))
                                   (dissoc :state :corgi-count :policy-count) 
                               )
                          )
          result-list-map-eligible   (filter #(:eligibility %) result-list-map)
          result-list-map-ineligible (remove #(:eligibility %) result-list-map)
         ]
        (with-open [writer-eligible   (io/writer   "eligible-corgi-cover-applications.csv")
                    writer-ineligible (io/writer "ineligible-corgi-cover-applications.csv")]
            (csv/write-csv writer-eligible [["Name" "Eligibility"]] ) ; header
            (csv/write-csv writer-eligible (map vals result-list-map-eligible) )
            (csv/write-csv writer-ineligible [["Name" "Eligibility"]] ) ; header
            (csv/write-csv writer-ineligible (map vals result-list-map-ineligible) )
        )
    )
)
(create-files)

; Challenge 2 Part 3
(defn eligibility-reason [{:keys [state corgi-count]}]  
    (cond (not (or (pos? corgi-count) (contains? #{"IL", "WA", "NY", "CO"} state)))
              "The applicant has no corgis, and does not live in IL, WA, NY, or CO."
          (not (pos? corgi-count)) "The applicant has no corgis."
          (not (contains? #{"IL", "WA", "NY", "CO"} state)) "The applicant does not live in IL, WA, NY, or CO."
          :else nil 
    )   
)
(defn create-files []
    (let [application-list-map (read-file)
          result-list-map (for [application-map application-list-map] 
                               (-> (assoc application-map :reason (eligibility-reason application-map))
                                   (dissoc :state :corgi-count :policy-count) 
                               )
                          )
          result-list-map-eligible   (remove #(:reason %) result-list-map)
          result-list-map-ineligible (filter #(:reason %) result-list-map)
         ]
        (with-open [writer-eligible   (io/writer   "eligible-corgi-cover-applications.csv")
                    writer-ineligible (io/writer "ineligible-corgi-cover-applications.csv")]
            (csv/write-csv writer-eligible [["Name" "Reason"]] ) ; header
            (csv/write-csv writer-eligible (map vals result-list-map-eligible) )
            (csv/write-csv writer-ineligible [["Name" "Reason"]] ) ; header
            (csv/write-csv writer-ineligible (map vals result-list-map-ineligible) )
        )   
    )   
)
(create-files)

; Challenge 2 Part 4
(require '[clojure.data.json :as json])
(defn write-to-json-files []
    (let [application-list-map (read-file)
          result-list-map (for [application-map application-list-map] 
                               (-> (assoc application-map :reason (eligibility-reason application-map))
                                   (dissoc :state :corgi-count :policy-count) 
                               )
                          )
          result-list-map-eligible   (remove #(:reason %) result-list-map)
          result-list-map-ineligible (filter #(:reason %) result-list-map)
         ]
        (with-open [writer-eligible   (io/writer   "eligible-corgi-cover-applications.json")
                    writer-ineligible (io/writer "ineligible-corgi-cover-applications.json")]
            (json/write result-list-map-eligible   writer-eligible)
            (json/write result-list-map-ineligible writer-ineligible)
        )
    )
) 
(write-to-json-files)


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
