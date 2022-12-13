advent of code - day 13
================
chad allison \| 13 december 2022

### loading tidyverse

``` r
library(tidyverse)
library(glue)
library(here)
```

### loading input data

``` r
input = data.frame(x = readLines("day13_input.txt"))

input |>
  pull(x) |>
  head(5)
```

    ## [1] "[[10],[4,3,5,[[],8,[4,3,10,9,4]],5],[[[6],[8]],9,1],[4,8,2,[[],[2,1,7]],6]]"                                 
    ## [2] "[[5],[10]]"                                                                                                  
    ## [3] ""                                                                                                            
    ## [4] "[[9,2],[[9,[2,10,3,1],[9,0,6],[5,8,0]],4,4,10,8],[[[0,0],[4,9,4]],7,6,2,6],[[[1]],[[2,10,4,8,5]]],[7,5,6,1]]"
    ## [5] "[[[[3,0,2,9],[],[6,10,6],[0,4,5,2]]]]"

### creating function to determine if values are ordered or not

``` r
compare = function(x, y) {
  if (is.numeric(x) & is.numeric(y)) {
    return(sign(y - x))
  }
  if (!is.list(x)) x = list(x)
  if (!is.list(y)) y = list(y)
  for (i in seq_len(length(x))) {
    if (i > length(y)) {
      return(-1) # wrong order
    }
    cmp = compare(x[[i]], y[[i]])
    if (cmp != 0) {
      return(cmp)
    }
  }
  if (length(x) == length(y)) {
    return(0) # check next
  }
  return(1) # right order
}
```

### collecting data packets

``` r
packets = input |>
  mutate(x = str_replace_all(x, "\\[", "list("),
         x = str_replace_all(x, "\\]", ")"),
         pair = cumsum(x == "") + 1) |>
  filter(x != "") |>
  mutate(packet = map(x, ~ eval(parse(text = .))))

packets |>
  select(pair, packet) |>
  head()
```

    ##   pair
    ## 1    1
    ## 2    1
    ## 3    2
    ## 4    2
    ## 5    3
    ## 6    3
    ##                                                                                                          packet
    ## 1                                            10, 4, 3, 5, 8, 4, 3, 10, 9, 4, 5, 6, 8, 9, 1, 4, 8, 2, 2, 1, 7, 6
    ## 2                                                                                                         5, 10
    ## 3 9, 2, 9, 2, 10, 3, 1, 9, 0, 6, 5, 8, 0, 4, 4, 10, 8, 0, 0, 4, 9, 4, 7, 6, 2, 6, 1, 2, 10, 4, 8, 5, 7, 5, 6, 1
    ## 4                                                                              3, 0, 2, 9, 6, 10, 6, 0, 4, 5, 2
    ## 5                                                                                                    5, 5, 6, 0
    ## 6                                                                                                 5, 5, 6, 0, 0

### comparing pairs

``` r
packets_evaluated = packets |>
  group_by(pair) |>
  summarise(packet1 = packet[1],
            packet2 = packet[2]) |>
  mutate(cmp = map2_dbl(packet1, packet2, compare))
```

### part 1 solution

``` r
packets_evaluated |>
  filter(cmp == 1) |>
  summarise(sum = sum(pair)) |>
  pull(sum)
```

    ## [1] 5196

### identifying where dividers sit

``` r
packets_evaluated = packets |>
  mutate(div1 = map_dbl(packet, compare, list(list(2))),
         div2 = map_dbl(packet, compare, list(list(6))))
```

### part 2 solution

``` r
packets_evaluated |>
  summarise(position_divider_1 = sum(div1 == 1) + 1,
            position_divider_2 = sum(div2 == 1) + 2,
            prod = position_divider_1 * position_divider_2) |>
  pull(prod)
```

    ## [1] 22134
