# Bamboo
A minimal, "pandas-like" library for Clojure, implemented on numcloj, a "numpy-like" library for Clojure. 

## Usage
The main user namespaces are:
* `bamboo.core` for creating top-level "pandas objects" (stored as Clojure maps) such as dataframes, various types of indices, etc. 
* `bamboo.dataframe` for operating on a dataframe, eg `drop*` rows and columns, `take*` rows and columns, etc.
* `numcloj.core` for creating and manipulating "ndarrays" (stored as native java arrays) and operating on them, eg efficiently (through type-hints) map a function over an array using `vectorize`, etc
* `numcloj.ndarray` for additional operations on "ndarrays"

>During development, it is highly recommended that you use the clojure.spec 
"checked" version of the libraries, eg. `bamboo.checked.core`, that validate 
function arguments against the specification. In production, revert to using 
the unchecked versions for improved performance.

| SciPy Libraries (Python) | Bamboo Libraries (Clojure) | Supported Operations |
| - | - | - |
| `pandas` | `bamboo.core` | `array, dataframe, date-range, index, rangeindex, read-csv` |
| `pandas.DataFrame` | `bamboo.dataframe` | `applymap, at, drop*, equals, iat, iloc, itertuples, loc,     sort-values, take*, to-string, transpose`  |
| `pandas.Index` | `bamboo.index` | `array, copy, drop*, dtypes, equals, get-loc, map*, T, take*, to-list, to-native-types,  to-numpy` |
| `numpy` | `numcloj.core` | `all, amax, any, argmax, argmin, argsort, array, array-equal, asarray, copy, copyto, count-nonzero, delete, empty+, empty-like, flatnonzero, frombuffer, full, full-like, isnan, ones, ones-like, put, recarray, rec.fromarrays, take*, vectorize, zeros, zeros-like` |
| `numpy.ndarray` | `numcloj.ndarray` | `argsort, copy, fill, item, itemset, put, take*, tolist` |

_Equivalent SciPy libraries in Bamboo_

### bamboo.core
The main namespace for top-level, pandas-like operations in the `bamboo` 
library is `bamboo.core` or, alternatively, use `bamboo.checked.core` for 
rigorous function argument checking. 

```clojure
(require '[bamboo.checked.core :as pd]
         '[bamboo.dataframe :refer [to-string]])
```

Create a dataframe from a CSV file:
```clojure
user=> (def df (pd/read-csv "example.csv"))
#'user/df
user=> (println (to-string df))     
  a b  c  d  
0 1 2  3  NaN
1 5 6  7  8.0
2 9 10 11 NaN
```
Create a dataframe from collection data, with default columns and index:
```clojure
user=> (def df (pd/dataframe (partition 5 (range 20)))) 
#'user/df
user=> (println (to-string df))
  0 1 2  3 
0 0 5 10 15
1 1 6 11 16
2 2 7 12 17
3 3 8 13 18
4 4 9 14 19
```
Create a dataframe from collection data, named columns, 
and datetimes for the index:
```clojure
user=> (def df (pd/dataframe (partition 5 (range 20)) 
                :columns ["w" "x" "y" "z"] 
                :index (pd/date-range :start "2019-01-01" 
                                      :periods 5 
                                      :freq "min")))
#'user/df
user=> (println (to-string df))
                    w x y  z 
2019-01-01T00:00:00 0 5 10 15
2019-01-01T00:01:00 1 6 11 16
2019-01-01T00:02:00 2 7 12 17
2019-01-01T00:03:00 3 8 13 18
2019-01-01T00:04:00 4 9 14 19
```
### bamboo.dataframe
Once a dataframe has been created, it can be manipulated using `bamboo.dataframe`
(or `bamboo.checked.dataframe`).

```clojure
(require '[bamboo.checked.core :as pd]
         '[bamboo.checked.dataframe :as dataframe])
```

Sort a dataframe by column(s):
```clojure
user=> (def df (pd/dataframe (repeatedly 5 (partial shuffle [1 2 3]))))
#'user/df
user=> (println (dataframe/tostring df))
  0 1 2 3 4
0 2 3 2 3 3
1 3 1 3 1 1
2 1 2 1 2 2

user=> (def sorted (dataframe/sort-values df [0]))
#'user/sorted
  0 1 2 3 4
2 1 2 1 2 2
0 2 3 2 3 3
1 3 1 3 1 1

```

## Testing
`clj -A:test`

