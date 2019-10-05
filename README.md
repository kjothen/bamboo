# Bamboo
A pandas dataframe-like library. Includes numcloj, a numpy-like library. 

**Note:** *Neither bamboo or numcloj are functionally complete*

## Usage
```clojure
(require '[bamboo.core :as pd]
         '[bamboo.dataframe :as dataframe]
         '[bamboo.pprint :as pp])

(def df (pd/read-csv "example.csv"))
```
```bash
user=> (pp/pprint (dataframe/drop df "b"))

| a |  c |   d |
|---+----+-----|
| 1 |  3 | NaN |
| 5 |  7 | 8.0 |
| 9 | 11 | NaN |
```

## Testing
`clj -A:test -d "src/test/clojure"`

