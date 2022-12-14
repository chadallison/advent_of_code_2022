advent of code - day 14
================
chad allison \| 14 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input

``` r
input = data.frame(x = readLines("day14_input.txt"))

head(input) |>
  pull(x)
```

    ## [1] "477,140 -> 481,140"                                                                                                                                                                                                                          
    ## [2] "468,149 -> 472,149"                                                                                                                                                                                                                          
    ## [3] "482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30"
    ## [4] "465,152 -> 469,152"                                                                                                                                                                                                                          
    ## [5] "482,30 -> 482,23 -> 482,30 -> 484,30 -> 484,20 -> 484,30 -> 486,30 -> 486,26 -> 486,30 -> 488,30 -> 488,23 -> 488,30 -> 490,30 -> 490,20 -> 490,30 -> 492,30 -> 492,25 -> 492,30 -> 494,30 -> 494,29 -> 494,30 -> 496,30 -> 496,24 -> 496,30"
    ## [6] "501,140 -> 505,140"

### finding blocked spots

``` r
blocked = input |>
  mutate(row = row_number()) |>
  separate_rows(x, sep = " -> ") |>
  separate(x, into = c("x_start", "y_start"), convert = T) |>
  group_by(row) |>
  mutate(x_end = lead(x_start),
         y_end = lead(y_start)) |>
  filter(!is.na(x_end)) |>
  mutate(x = map2(x_start, x_end, seq),
         y = map2(y_start, y_end, seq)) |>
  unnest(c(x, y))

head(blocked)
```

    ## # A tibble: 6 x 7
    ## # Groups:   row [2]
    ##   x_start y_start   row x_end y_end     x     y
    ##     <int>   <int> <int> <int> <int> <int> <int>
    ## 1     477     140     1   481   140   477   140
    ## 2     477     140     1   481   140   478   140
    ## 3     477     140     1   481   140   479   140
    ## 4     477     140     1   481   140   480   140
    ## 5     477     140     1   481   140   481   140
    ## 6     468     149     2   472   149   468   149

### creating move list

``` r
moves = list(cbind(0, 1), # down
             cbind(-1, 1), # down & left
             cbind(1, 1)) # down & right

moves
```

    ## [[1]]
    ##      [,1] [,2]
    ## [1,]    0    1
    ## 
    ## [[2]]
    ##      [,1] [,2]
    ## [1,]   -1    1
    ## 
    ## [[3]]
    ##      [,1] [,2]
    ## [1,]    1    1

### creating function for next sand position

``` r
next_sand = function(m) {
  position = cbind(500, 0) # start
  moved = T
  while (moved) {
    moved = F
    for (move in moves) {
      if (m[position + move] == 0) {
        position = position + move
        moved = T
      }
    }
  }
  return(position)
}
```

### creating movement matrix

``` r
# 0 is empty
# 1 is rock
# 2 is sand
m = matrix(0, nrow = max(blocked$x) * 3, ncol = max(blocked$y) + 2)
m[cbind(blocked$x, blocked$y)] = 1
m[, max(blocked$y) + 2] = 1

m[1:5, 1:5]
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    0    0    0    0    0
    ## [2,]    0    0    0    0    0
    ## [3,]    0    0    0    0    0
    ## [4,]    0    0    0    0    0
    ## [5,]    0    0    0    0    0

### making moves

``` r
while (T) {
  pos = next_sand(m)
  m[pos] = 2
  if (pos[2] == 0) {
    break
  }
}
```

### solution

``` r
sum(m == 2) + 1
```

    ## [1] 27426
