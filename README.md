# Bamboo
A pandas dataframe-like library. Includes numcloj, a numpy-like library. 

**Note:** *Neither bamboo or numcloj are functionally complete*

## Usage
```clojure
% clj -A:rebel
user=> (require '[bamboo.checked.core :as pd] '[bamboo.checked.dataframe :as dataframe])
nil
user=> (def df (pd/read-csv "example.csv"))
#'user/df
user=> (println (dataframe/to-string df))
a b  c  d  
1 2  3  NaN
5 6  7  8.0
9 10 11 NaN
nil
user=> (println (dataframe/to-string df :columns ["b" "d"]))
b  d  
2  NaN
6  8.0
10 NaN
nil
```

## Testing
`clj -A:test`

