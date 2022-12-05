advent of code - day 4
================
chad allison \| 4 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading and previewing input data

``` r
input = readLines("day4_input.txt")
df = data.frame(pairs = input)

head(df)
```

    ##         pairs
    ## 1    7-7,8-42
    ## 2   2-95,2-94
    ## 3 10-54,33-90
    ## 4 23-24,24-40
    ## 5  1-48,12-47
    ## 6   9-27,9-26

### splitting the pairs

``` r
df = df |>
  separate(pairs, into = c("p1", "p2"), sep = ",")

head(df)
```

    ##      p1    p2
    ## 1   7-7  8-42
    ## 2  2-95  2-94
    ## 3 10-54 33-90
    ## 4 23-24 24-40
    ## 5  1-48 12-47
    ## 6  9-27  9-26

### further splitting the pairs

``` r
df = df |>
  separate(p1, into = c("min1", "max1"), sep = "-") |>
  separate(p2, into = c("min2", "max2"), sep = "-") |>
  mutate_all(as.numeric)

head(df)
```

    ##   min1 max1 min2 max2
    ## 1    7    7    8   42
    ## 2    2   95    2   94
    ## 3   10   54   33   90
    ## 4   23   24   24   40
    ## 5    1   48   12   47
    ## 6    9   27    9   26

### determining how many pair ranges fully contain the other

``` r
df = df |>
  mutate(p1_contains_p2 = ifelse(min1 <= min2 & max1 >= max2, 1, 0),
         p2_contains_p1 = ifelse(min2 <= min1 & max2 >= max1, 1, 0),
         same_pair = ifelse(min1 == min2 & max1 == max2, 1, 0))

head(df)
```

    ##   min1 max1 min2 max2 p1_contains_p2 p2_contains_p1 same_pair
    ## 1    7    7    8   42              0              0         0
    ## 2    2   95    2   94              1              0         0
    ## 3   10   54   33   90              0              0         0
    ## 4   23   24   24   40              0              0         0
    ## 5    1   48   12   47              1              0         0
    ## 6    9   27    9   26              1              0         0

### part 1 solution

``` r
sum(df$p1_contains_p2) + sum(df$p2_contains_p1) - sum(df$same_pair)
```

    ## [1] 433

### creating variable for no overlap, then overlap

``` r
df = df |>
  select(-same_pair) |>
  mutate(no_overlap = ifelse(max1 < min2 | min1 > max2, 1, 0),
         overlap = ifelse(no_overlap == 0, 1, 0))

head(df)
```

    ##   min1 max1 min2 max2 p1_contains_p2 p2_contains_p1 no_overlap overlap
    ## 1    7    7    8   42              0              0          1       0
    ## 2    2   95    2   94              1              0          0       1
    ## 3   10   54   33   90              0              0          0       1
    ## 4   23   24   24   40              0              0          0       1
    ## 5    1   48   12   47              1              0          0       1
    ## 6    9   27    9   26              1              0          0       1

### part 2 solution

``` r
sum(df$overlap)
```

    ## [1] 852
