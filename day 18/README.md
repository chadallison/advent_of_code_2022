advent of code - day 18
================
chad allison \| 18 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input data

``` r
input = readLines("input.txt")
input[1:50]
```

    ##  [1] "7,2,8"    "15,14,4"  "3,12,6"   "18,9,8"   "6,13,4"   "7,16,14" 
    ##  [7] "7,2,7"    "14,7,3"   "15,6,5"   "12,7,2"   "2,7,6"    "11,10,17"
    ## [13] "14,8,3"   "6,16,11"  "8,4,15"   "12,10,2"  "7,5,5"    "12,3,12" 
    ## [19] "11,2,11"  "13,16,11" "4,6,12"   "5,2,9"    "2,10,11"  "12,17,7" 
    ## [25] "12,4,3"   "10,8,2"   "4,6,4"    "6,6,2"    "17,9,9"   "8,8,17"  
    ## [31] "3,7,5"    "5,5,7"    "15,11,6"  "10,10,1"  "9,11,16"  "1,7,12"  
    ## [37] "16,16,7"  "15,12,14" "3,9,15"   "12,5,5"   "17,11,6"  "17,12,11"
    ## [43] "3,4,7"    "3,7,7"    "14,16,11" "10,9,17"  "13,6,4"   "13,6,15" 
    ## [49] "11,13,15" "3,14,13"

### formatting input data

``` r
df = data.frame(input) |>
  separate(input, into = c("a", "b", "c"), sep = ",", convert = T)

head(df)
```

    ##    a  b  c
    ## 1  7  2  8
    ## 2 15 14  4
    ## 3  3 12  6
    ## 4 18  9  8
    ## 5  6 13  4
    ## 6  7 16 14

### creating neighbors function

``` r
neighbors = function(a, b, c, mat) {
  x = (abs(mat$a - a) + abs(mat$b - b) + abs(mat$c - c)) == 1
  return(sum(x))
}
```

### part 1 solution

``` r
sum(6 - pmap_dbl(df, neighbors, mat = df))
```

    ## [1] 3454

### creating all possible spaces

``` r
possible_spaces = crossing(a = seq_len(max(df$a)),
                           b = seq_len(max(df$b)),
                           c = seq_len(max(df$c)))

possible_spaces[sample(1:nrow(possible_spaces), size = 10), ]
```

    ## # A tibble: 10 × 3
    ##        a     b     c
    ##    <int> <int> <int>
    ##  1     6     2     4
    ##  2     9     4    13
    ##  3     2    14    12
    ##  4     5     6    14
    ##  5     4    18     5
    ##  6     9     6    15
    ##  7    12    17     2
    ##  8    16    10     4
    ##  9     6    13    13
    ## 10     7    14    12

### creating next_points function & essential tibbles

``` r
next_points = function(x, y, z) {
  tibble(a = c(-1, 0, 0, 0, 0, 1) + x, 
         b = c(0, -1, 0, 0, 1, 0) + y, 
         c = c(0, 0, -1, 1, 0, 0) + z) |> 
    filter(a >= 0, b >= 0, c >= 0, a <= 20, b <= 20, c <= 20) |> 
    anti_join(df, by = c("a", "b", "c")) |> 
    anti_join(checked, by = c("a", "b", "c")) |> 
    anti_join(queue, by = c("a", "b", "c"))
}

checked = tibble(a = numeric(), b = numeric(), c = numeric())
queue = tibble(a = 0, b = 0, c = 0)
counter = 0
```

### finding filled spaces

``` r
while (nrow(queue) > 0) {
  counter = counter + 1
  if (counter %% 100 == 0) print(counter)
  x = next_points(queue$a[1], queue$b[1], queue$c[1])
  
  checked = checked |>
    bind_rows(queue |> slice(1))
  
  queue = queue |>
    slice(-1) |> 
    bind_rows(x)
}
```

    ## [1] 100
    ## [1] 200
    ## [1] 300
    ## [1] 400
    ## [1] 500
    ## [1] 600
    ## [1] 700
    ## [1] 800
    ## [1] 900
    ## [1] 1000
    ## [1] 1100
    ## [1] 1200
    ## [1] 1300
    ## [1] 1400
    ## [1] 1500
    ## [1] 1600
    ## [1] 1700
    ## [1] 1800
    ## [1] 1900
    ## [1] 2000
    ## [1] 2100
    ## [1] 2200
    ## [1] 2300
    ## [1] 2400
    ## [1] 2500
    ## [1] 2600
    ## [1] 2700
    ## [1] 2800
    ## [1] 2900
    ## [1] 3000
    ## [1] 3100
    ## [1] 3200
    ## [1] 3300
    ## [1] 3400
    ## [1] 3500
    ## [1] 3600
    ## [1] 3700
    ## [1] 3800
    ## [1] 3900
    ## [1] 4000
    ## [1] 4100
    ## [1] 4200
    ## [1] 4300
    ## [1] 4400
    ## [1] 4500
    ## [1] 4600
    ## [1] 4700
    ## [1] 4800
    ## [1] 4900
    ## [1] 5000
    ## [1] 5100
    ## [1] 5200
    ## [1] 5300
    ## [1] 5400
    ## [1] 5500
    ## [1] 5600
    ## [1] 5700
    ## [1] 5800
    ## [1] 5900
    ## [1] 6000

``` r
filled_spaces = possible_spaces |> 
  anti_join(checked, by = c("a", "b", "c")) |>
  anti_join(df, by = c("a", "b", "c")) |> 
  bind_rows(df)

filled_spaces[sample(1:nrow(filled_spaces), size = 10), ]
```

    ## # A tibble: 10 × 3
    ##        a     b     c
    ##    <int> <int> <int>
    ##  1     6    12     4
    ##  2    10     5    17
    ##  3     8    17     8
    ##  4     2     5    12
    ##  5    10     8    18
    ##  6     2     6    12
    ##  7     5     9     7
    ##  8     4     7    16
    ##  9    12    11     4
    ## 10     6     7     4

### part 2 solution

``` r
sum(6 - pmap_dbl(filled_spaces, neighbors, mat = filled_spaces))
```

    ## [1] 2014
