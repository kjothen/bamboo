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
user=> (pp/pprint df)

|   | a |  b |  c |   d |
|---+---+----+----+-----|
| 0 | 1 |  2 |  3 | NaN |
| 1 | 5 |  6 |  7 | 8.0 |
| 2 | 9 | 10 | 11 | NaN |

user=> (pp/pprint (dataframe/drop df ["b" "d"]))

|   | a |  c |
|---+---+----|
| 0 | 1 |  3 |
| 1 | 5 |  7 |
| 2 | 9 | 11 |
```

## Testing
`clj -A:test`

