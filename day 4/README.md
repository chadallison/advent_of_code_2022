advent of code - day 4
================
chad allison \| 4 december 2022

### loading tidyverse)

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

### saving original pairs and further splitting the pairs

``` r
p1 = df |>
  pull(p1)

p2 = df |>
  pull(p2)

df = df |>
  separate(p1, into = c("min1", "max1"), sep = "-") |>
  separate(p2, into = c("min2", "max2"), sep = "-")

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
         p2_contains_p1 = ifelse(min2 <= min1 & max2 >= max1, 1, 0)) |>
  mutate(one_contains_other = ifelse(p1_contains_p2 == 1 | p2_contains_p1 == 1, 1, 0))

head(df)
```

    ##   min1 max1 min2 max2 p1_contains_p2 p2_contains_p1 one_contains_other
    ## 1    7    7    8   42              1              0                  1
    ## 2    2   95    2   94              1              0                  1
    ## 3   10   54   33   90              0              0                  0
    ## 4   23   24   24   40              0              0                  0
    ## 5    1   48   12   47              1              0                  1
    ## 6    9   27    9   26              1              0                  1

### part 1 solution

``` r
sum(df$one_contains_other) ### 493 too high - am i double counting?
```

    ## [1] 493
