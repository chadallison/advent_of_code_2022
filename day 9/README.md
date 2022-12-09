advent of code - day 9
================
chad allison \| 9 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input data

``` r
input = readLines("day9_input.txt") |>
  strsplit(" ")

input[1:5]
```

    ## [[1]]
    ## [1] "L" "1"
    ## 
    ## [[2]]
    ## [1] "R" "1"
    ## 
    ## [[3]]
    ## [1] "D" "1"
    ## 
    ## [[4]]
    ## [1] "R" "2"
    ## 
    ## [[5]]
    ## [1] "D" "2"

### creating vectors for head and tail position, along with visited positions

``` r
head = c(0, 0)
tail = c(0, 0)
visited = list(tail)

visited
```

    ## [[1]]
    ## [1] 0 0

### making moves indicated in input, tracking visited positions

``` r
for (move in input) {
  for (step in seq(as.numeric(move[2]))) {
    if (move[1] == "R") head = head + c(1, 0)
    if (move[1] == "L") head = head + c(-1, 0)
    if (move[1] == "U") head = head + c(0, -1)
    if (move[1] == "D") head = head + c(0, 1)
    if (max(abs(tail - head)) > 1) tail = tail + sign(head - tail)
    visited = append(visited, list(tail))
  }
}

visited[sample(length(input), 5)]
```

    ## [[1]]
    ## [1] -69  11
    ## 
    ## [[2]]
    ## [1] -39  10
    ## 
    ## [[3]]
    ## [1]  4 10
    ## 
    ## [[4]]
    ## [1]  0 -1
    ## 
    ## [[5]]
    ## [1] -3  3

### part 1 solution

``` r
length(unique(visited))
```

    ## [1] 6269

### creating vector for longer rope

``` r
rope = rep(list(c(0, 0)), 10)
visited = rope[10]

visited
```

    ## [[1]]
    ## [1] 0 0

### making input moves with longer rope

``` r
for (move in input) {
  for (step in seq(as.numeric(move[2]))) {
    if (move[1] == "R") rope[[1]] = rope[[1]] + c(1, 0)
    if (move[1] == "L") rope[[1]] = rope[[1]] + c(-1, 0)
    if (move[1] == "U") rope[[1]] = rope[[1]] + c(0, 1)
    if (move[1] == "D") rope[[1]] = rope[[1]] + c(0, -1)
    for (i in seq(rope)[-1]) {
      if (max(abs(rope[[i]] - rope[[i - 1]])) > 1) {
        rope[[i]] = rope[[i]] + sign(rope[[i - 1]] - rope[[i]])
      }
    }
    visited = append(visited, rope[10])
  }
}

visited[sample(length(input), 5)]
```

    ## [[1]]
    ## [1] -2 -8
    ## 
    ## [[2]]
    ## [1]   9 -13
    ## 
    ## [[3]]
    ## [1]  -2 -17
    ## 
    ## [[4]]
    ## [1] -7 18
    ## 
    ## [[5]]
    ## [1] -73 -15

### part 2 solution

``` r
length(unique(visited))
```

    ## [1] 2557
