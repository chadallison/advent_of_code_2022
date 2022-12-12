advent of code - day 12
================
chad allison \| 12 december 2022

### loading `tidyverse` and `igraph`

``` r
library(tidyverse)
library(igraph)
```

### loading input data

``` r
input = readLines("day12_input.txt")
input[1:5]
```

    ## [1] "abcccccccccccccccccaaccccccccccccaaaaaaaacccccccccccaaaaaccccaaaaaaccaaaaaaaaaaaaaaaaaccccccccccccccccaaacccccaaaaaaaacccaaaccccccccccccccccccccccccccccccccccccccccccccccccccaaaaa"
    ## [2] "abccccccccccccccccaaacaacccccccccccaaaacccccccccccccaaaaaacccaaaaaaccaaaaaaaaaaaaaaaaaaaacccccccccaaacaaacccccaaaaaaaaaccaaaaccccccccccccccccccccccccccccccccccccccccccccccccaaaaaa"
    ## [3] "abcccccccccccccccccaaaaacccccccccccaaaaaccccccccccccaaaaaaccccaaaacccaaaacccaaaaaaaaaaaaacccccccccaaaaaaaaaacccaaaaaaaaccaaaaccccccccccccccccccccccccccccccccccaaacccccccccccaaaaaa"
    ## [4] "abcccccccccccccccaaaaaacccccccccccaaacaaccaaccccccccaaaaaaccccaaaacccaaaccccaaaaaaaaaaaaaccccccccccaaaaaaaaaccaaaaaacccccaaacccccccccccccccccccccccccccccccccccaaaccccccccccccccaaa"
    ## [5] "abcccccccccccccccaaaaaaaacccccccccaacccacaaacaacccccccaaccccccaccaccccccccaaaaaaaaaaaaccccccccccccccaaaaaaacccaaaaaaacccccccccccccaacccccccccccccccccccccccccccaaaccccccccccccccaaa"

### formatting input

``` r
x = input |>
  strsplit("") |>
  sapply(match, c("S", letters, "E"))

x[1:10, 1:10]
```

    ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ##  [1,]    2    2    2    2    2    2    2    2    2     2
    ##  [2,]    3    3    3    3    3    3    3    3    3     3
    ##  [3,]    4    4    4    4    4    4    4    2    2     2
    ##  [4,]    4    4    4    4    4    4    4    2    2     2
    ##  [5,]    4    4    4    4    4    4    2    2    2     2
    ##  [6,]    4    4    4    4    4    4    2    2    2     4
    ##  [7,]    4    4    4    4    4    4    4    4    4     4
    ##  [8,]    4    4    4    4    4    4    4    4    4     4
    ##  [9,]    4    4    4    4    4    4    4    4    2     4
    ## [10,]    4    4    4    4    4    4    4    2    2     2

### computing edge list

``` r
edgelist = lapply(seq(x), \(s) {
  i = row(x)[s]
  j = col(x)[s]
  height = x[i, j]
  walkable = which(abs(row(x) - i) + abs(col(x) - j) == 1 & x - height <= 1)
  expand.grid(s, walkable)
})

edgelist[[1]]
```

    ##   Var1 Var2
    ## 1    1    2
    ## 2    1  180

### creating graph object from `igraph`

``` r
g = do.call(rbind, edgelist) |>
  as.matrix() |>
  graph_from_edgelist()

g
```

    ## IGRAPH 3e05849 D--- 7339 26868 -- 
    ## + edges from 3e05849:
    ##  [1]  1->  2  1->180  2->  1  2->  3  2->181  3->  2  3->  4  3->182  4->  3
    ## [10]  4->  5  4->183  5->  4  5->  6  5->184  6->  5  6->  7  6->185  7->  6
    ## [19]  7->  8  7->186  8->  7  8->  9  8->187  9->  8  9-> 10  9->188 10->  9
    ## [28] 10-> 11 10->189 11-> 10 11-> 12 11->190 12-> 11 12-> 13 12->191 13-> 12
    ## [37] 13-> 14 13->192 14-> 13 14-> 15 14->193 15-> 14 15-> 16 15->194 16-> 15
    ## [46] 16-> 17 16->195 17-> 16 17-> 18 17->196 18-> 17 18-> 19 18->197 19-> 18
    ## [55] 19-> 20 19->198 20-> 21 20->199 21-> 20 21->200 22-> 21 22-> 23 22->201
    ## [64] 23-> 22 23-> 24 23->202 24-> 23 24-> 25 24->203 25-> 24 25-> 26 25->204
    ## [73] 26-> 25 26-> 27 26->205 27-> 26 27-> 28 27->206 28-> 27 28-> 29 28->207
    ## + ... omitted several edges

### part 1 solution

``` r
shortest_paths(g, from = which.min(x), to = which.max(x))$vpath[[1]] |>
  tail(-1) |>
  length()
```

    ## [1] 484

### finding shortest path

``` r
sol = sapply(which(x <= 2), \(i) {
  steps = shortest_paths(g, from = i, to = which.max(x))$vpath[[1]] |>
    suppressWarnings() |>
    tail(-1) |>
    length()
  
  ifelse(steps == 0, Inf, steps)
})

sol[sol != Inf]
```

    ##  [1] 504 503 502 501 500 499 498 500 501 497 497 498 499 500 496 496 497 498 499
    ## [20] 495 495 496 497 494 494 495 496 497 493 493 494 495 496 492 492 493 494 495
    ## [39] 496 491 491 490 489 488 487 486 485 484 483 482 481 480 479 478 479 480 481
    ## [58] 482 483 484 485 486 486 487 487 487 488 488 488 489 489 489 490 488 488 489
    ## [77] 487 488

### part 2 solution

``` r
min(sol)
```

    ## [1] 478
