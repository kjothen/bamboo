# Bamboo
A minimal, "pandas-like" library for Clojure, implemented on numcloj, a "numpy-like" library for Clojure. 

## Usage
The main user namespaces are:
* `bamboo.core` for creating top-level "pandas objects" (stored as Clojure maps) such as dataframes, various types of indices, etc. 
* `bamboo.dataframe` for operating on a dataframe, eg `drop*` rows and columns, `take*` rows and columns, etc.
* `numcloj.core` for creating and manipulating "ndarrays" (stored as native java arrays) and operating on them, eg efficiently (through type-hints) map a function over an array using `vectorize`, etc
* `numcloj.ndarray` for additional operations on "ndarrays"

>During development, it is highly recommended that you use the clojure.spec 
"checked" version of the libraries, eg. `bamboo.checked.core`, toalidate 
function arguments against the specification. In production, revert to using 
the unchecked versions for improved performance.

| SciPy Libraries (Python) | Bamboo Libraries (Clojure) | Supported Operations |
| - | - | - |
| `pandas` | `bamboo.core` | `array, dataframe, date-range, index, rangeindex, read-csv` |
| `pandas.DataFrame` | `bamboo.dataframe` | `applymap, at, drop*, equals, iat, iloc, itertuples, loc,     sort-values, take*, to-string, transpose`  |
| `pandas.Index` | `bamboo.index` | `array, copy, drop*, dtypes, equals, get-loc, map*, slice-locs, T, take*, to-list, to-native-types,  to-numpy` |
| `numpy` | `numcloj.core` | `amax, argmax, argmin, argsort, array, array-equal, asarray, copy, copyto, count-nonzero, delete, empty*, empty-like, equal, flatnonzero, frombuffer, full, full-like, greater, greater-equal, isnan, less, less-equal, logical-and, logical-not, logical-or, not-equal, ones, ones-like, put, recarray, rec.fromarrays, take*, vectorize, zeros, zeros-like` |
| `numpy.ndarray` | `numcloj.ndarray` | `argsort, copy, fill, item, itemset, put, take*, tolist` |

_Equivalent SciPy libraries in Bamboo_

### bamboo
The main namespace for top-level, pandas-like operations in the `bamboo` 
library is `bamboo.core` or, alternatively, use `bamboo.checked.core` for 
rigorous function argument checking. Dataframe operations are in the 
`bamboo.dataframe` library (with checked versions in `bamboo.checked.dataframe`).

```python
# python
import pandas as pd
```
```clojure
; clojure
(require '[bamboo.checked.core :as pd]
         '[bamboo.checked.dataframe :as dataframe]
         '[bamboo.lang :refer [slice]])
```

Create a dataframe from a CSV file:

```python
# python
df = pd.read_csv("kepler.csv.gz", skiprows=53)
```
```clojure
; clojure
user=> (def df (pd/read-csv "kepler.csv.gz" :skiprows 53))
#'user/df
```

Show a snippet of the dataframe:

```Python
# python
print (df.to_string(max_cols=6, max_rows=5, show_dimensions=True))
```
```clojure
; clojure
user=> (pd/show df :max-cols 6 :max-rows 4 :show-dimensions true)
```
```bash    
         kepid kepoi_name  kepler_name  ...         ra       dec koi_kepmag
0     10797460  K00752.01 Kepler-227 b  ...  291.93423 48.141651     15.347
1     10797460  K00752.02 Kepler-227 c  ...  291.93423 48.141651     15.347
...        ...        ...          ...  ...        ...       ...        ...
9562  10155286  K07988.01               ...  296.76288 47.145142     10.998
9563  10156110  K07989.01               ...  297.00977 47.121021     14.826

[9564 rows x 49 columns]
nil
```

Show all the columns:

```python
# python
df.columns
```
```clojure
; clojure
(pd/show (:columns df))
```
```bash
Index(['kepid', 'kepoi_name', 'kepler_name', 'koi_disposition', 
       'koi_pdisposition', 'koi_score', 'koi_fpflag_nt', 'koi_fpflag_ss', 
       'koi_fpflag_co', 'koi_fpflag_ec', 'koi_period', 'koi_period_err1', 
       'koi_period_err2', 'koi_time0bk', 'koi_time0bk_err1', 
       'koi_time0bk_err2', 'koi_impact', 'koi_impact_err1', 'koi_impact_err2', 
       'koi_duration', 'koi_duration_err1', 'koi_duration_err2', 'koi_depth', 
       'koi_depth_err1', 'koi_depth_err2', 'koi_prad', 'koi_prad_err1', 
       'koi_prad_err2', 'koi_teq', 'koi_teq_err1', 'koi_teq_err2', 'koi_insol', 
       'koi_insol_err1', 'koi_insol_err2', 'koi_model_snr', 'koi_tce_plnt_num', 
       'koi_tce_delivname', 'koi_steff', 'koi_steff_err1', 'koi_steff_err2', 
       'koi_slogg', 'koi_slogg_err1', 'koi_slogg_err2', 'koi_srad', 
       'koi_srad_err1', 'koi_srad_err2', 'ra', 'dec', 'koi_kepmag'], 
      dtype='object')
```

Show data for specific columns: 

```python
# python
print (df.to_string(columns=['kepid', 'kepoi_name', 'kepler_name', 'koi_disposition', 'koi_score'], max_rows=4))
```
```clojure
; clojure
(pd/show df :columns ["kepid" "kepoi_name" "kepler_name" "koi_disposition", "koi_score"] :max-rows 4)
```
```bash
         kepid kepoi_name  kepler_name koi_disposition koi_score
0     10797460  K00752.01 Kepler-227 b       CONFIRMED       1.0
1     10797460  K00752.02 Kepler-227 c       CONFIRMED     0.969
...        ...        ...          ...             ...       ...
9562  10155286  K07988.01                    CANDIDATE     0.092
9563  10156110  K07989.01               FALSE POSITIVE       0.0
nil
```

Select confirmed exoplanets with a disposition score equal to 1.0:

```python
# python
df_confirmed = df[(df["koi_disposition"] == "CONFIRMED") & (df["koi_score"] == 1.0)]
print (df_confirmed.to_string(columns=['kepid', 'kepoi_name', 'kepler_name', 'koi_pdisposition', 'koi_score', 'koi_period'], max_rows=4))
```
```clojure
; clojure
(require '[bamboo.dataframe :as dataframe])
(let [dfx (partial dataframe/expr df)
      cond1 (pd/equal (dfx "koi_disposition") "CONFIRMED")
      cond2 (pd/equal (dfx "koi_score") 1.0)]
  (pd/show (dfx (pd/logical-and cond1 cond2)) :columns ["kepid" "kepoi_name" "kepler_name" "koi_disposition", "koi_score"] :max-rows 4))
```
```bash
         kepid kepoi_name   kepler_name koi_disposition koi_score
0     10797460  K00752.01  Kepler-227 b       CONFIRMED       1.0
4     10854555  K00755.01  Kepler-664 b       CONFIRMED       1.0
...        ...        ...           ...             ...       ...
7612  11125797  K03371.02 Kepler-1482 b       CONFIRMED       1.0
8817   7350067  K06863.01 Kepler-1646 b       CONFIRMED       1.0
```

Show columns upto and include 'koi_score':

```python
# python
print (df.loc[:, :'koi_score'].to_string(max_rows=4))
```
```clojure
; clojure
(pd/show (dataframe/loc df (slice) (slice :end "koi_score")) :max-rows 4)
```
```bash
         kepid kepoi_name  kepler_name koi_disposition koi_pdisposition koi_score
0     10797460  K00752.01 Kepler-227 b       CONFIRMED        CANDIDATE       1.0
1     10797460  K00752.02 Kepler-227 c       CONFIRMED        CANDIDATE     0.969
...        ...        ...          ...             ...              ...       ...
9562  10155286  K07988.01                    CANDIDATE        CANDIDATE     0.092
9563  10156110  K07989.01               FALSE POSITIVE   FALSE POSITIVE       0.0
```

Take rows and columns of interest:

```python
# python
df_interest = df.loc[(df["koi_disposition"] == "CONFIRMED") & (df["koi_score"] == 1.0), 'kepid':'koi_score']
print(df_interest.to_string(max_rows=4))
```
```clojure
; clojure
(def df-interest
  (let [dfx (partial dataframe/expr df)
        cond1 (pd/equal (dfx "koi_disposition") "CONFIRMED")
        cond2 (pd/equal (dfx "koi_score") 1.0)]
    (dataframe/loc df (pd/logical-and cond1 cond2) (slice :end "koi_score"))))
(pd/show df-interest :max-rows 4)
```

Dataframes don't have to come from CSV files: they can be created from data too. 
Create a dataframe from collection data, named columns, 
and periodic datetimes for the index:

```clojure
; clojure
user=> (def df (pd/dataframe (partition 5 (range 20)) 
                :columns ["w" "x" "y" "z"] 
                :index (pd/date-range :start "2019-01-01" 
                                      :periods 5 
                                      :freq "min")))
#'user/df
user=> (pd/show df)
                    w x y  z 
2019-01-01T00:00:00 0 5 10 15
2019-01-01T00:01:00 1 6 11 16
2019-01-01T00:02:00 2 7 12 17
2019-01-01T00:03:00 3 8 13 18
2019-01-01T00:04:00 4 9 14 19
nil
```

## Testing
`clj -A:test`

