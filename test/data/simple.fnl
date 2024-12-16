{:identity (fn [x] "foo" x)
 :nothing (fn [] (values))
 :rest (fn [x ...] ...)
 :table (fn [{: foo : bar}] {:foo bar :bar foo})
 :vector (fn [[foo bar]] [bar foo])
 :four 4
 :true true
 :false false
 :plain-table {:foo 1}
 :plain-vector [:foo 1]}
