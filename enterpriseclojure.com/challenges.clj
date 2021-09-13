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
(defn accept_map [{:keys [state corgi-count policy-count]}]
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
