advent of code - day 17
================
chad allison \| 17 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input data

``` r
input = readLines("day17_input.txt")
substr(input, 1, 150)
```

    ## [1] ">>>><<<<>>><<>>><<<<>><<>>>><><><><<<<><<>>>><<<<>>><><<><<>>>><<>>>><><<>>><>>><<<<>>><<<><<<<>>><<<<>>>><<<<>><<<<><<<<>><<>>><>>>><>>><<>>><<<<>>><"

### formatting input

``` r
input = input |>
  strsplit("") |>
  unlist()

input[1:50]
```

    ##  [1] ">" ">" ">" ">" "<" "<" "<" "<" ">" ">" ">" "<" "<" ">" ">" ">" "<" "<" "<"
    ## [20] "<" ">" ">" "<" "<" ">" ">" ">" ">" "<" ">" "<" ">" "<" ">" "<" "<" "<" "<"
    ## [39] ">" "<" "<" ">" ">" ">" ">" "<" "<" "<" "<" ">"

### creating rocks list

``` r
rocks = list(r1 = matrix(1, ncol = 4),
             r2 = matrix(c(0, 1, 0, 1, 1, 1, 0, 1, 0),
                         nrow = 3, ncol = 3, byrow = T),
             r3 = matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 1),
                         nrow = 3, ncol = 3, byrow = 3),
             r4 = matrix(1, nrow = 4),
             r5 = matrix(1, ncol = 2, nrow = 2))

rocks[1:2]
```

    ## $r1
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    1    1    1
    ## 
    ## $r2
    ##      [,1] [,2] [,3]
    ## [1,]    0    1    0
    ## [2,]    1    1    1
    ## [3,]    0    1    0

### setting up part 1

``` r
mat <- matrix(1, ncol = 7)
jets <- input

rock_list <- rocks |> lapply(\(.x) {
  x <- which(.x == 1, arr.ind = T)
  x[, 2] <- x[, 2] + 2
  x
  })

rock_num <- 1
rock_type <- 1
jet_num <- 1

rock_list[1:2]
```

    ## $r1
    ##      row col
    ## [1,]   1   3
    ## [2,]   1   4
    ## [3,]   1   5
    ## [4,]   1   6
    ## 
    ## $r2
    ##      row col
    ## [1,]   2   3
    ## [2,]   1   4
    ## [3,]   2   4
    ## [4,]   3   4
    ## [5,]   2   5

### part 1 looping

``` r
while (rock_num <= 2022) {
  pos <- rock_list[[rock_type]]
  mat <- rbind(matrix(0, nrow = 3 + max(pos[, 1]), ncol = 7), mat)  
  
  repeat {
    j <- jets[jet_num]
    if (j == ">" && all((pos[, 2]) < 7) &&
        all(mat[cbind(pos[, 1], (pos[, 2] + 1))] == 0)) pos[, 2] <- pos[, 2] + 1
    if (j == "<" && all((pos[, 2]) > 1) && all(mat[cbind(pos[, 1], (pos[, 2] - 1))] == 0)) pos[, 2] <- pos[, 2] - 1
    jet_num <- jet_num %% length(jets) + 1

    if (any(mat[cbind((pos[, 1] + 1), pos[, 2])] == 1)) break
    pos[, 1] <- pos[, 1] + 1
  }
  
  mat[pos] <- 1
  mat <- mat[apply(mat, 1,sum) != 0, ] |> unname()
  rock_num <- rock_num + 1
  rock_type <- (rock_type %% 5) + 1
}
```

### part 1 solution

``` r
nrow(mat) - 1
```

    ## [1] 3106

### setting up part 1

``` r
mat <- matrix(1, ncol = 7)
jets <- input

rock_list <- rocks |> lapply(\(.x) {
  x <- which(.x == 1, arr.ind = TRUE)
  x[,2] <- x[,2] + 2
  x
  })

rock_num <- 1
rock_type <- 1
jet_num <- 1
heights <- integer()

rock_list[1:2]
```

    ## $r1
    ##      row col
    ## [1,]   1   3
    ## [2,]   1   4
    ## [3,]   1   5
    ## [4,]   1   6
    ## 
    ## $r2
    ##      row col
    ## [1,]   2   3
    ## [2,]   1   4
    ## [3,]   2   4
    ## [4,]   3   4
    ## [5,]   2   5

### part 2 looping

``` r
while(rock_num <= 4000){
  pos <- rock_list[[rock_type]]
  mat <- rbind(matrix(0, nrow = 3 + max(pos[,1]), ncol = 7), mat)  
  
  repeat {
    j <- jets[jet_num]
    if(j == ">" && all((pos[,2]) < 7) && all(mat[cbind(pos[,1],(pos[,2] + 1))]==0)) pos[,2] <- pos[,2] + 1
    if(j == "<" && all((pos[,2]) > 1) && all(mat[cbind(pos[,1],(pos[,2] - 1))]==0)) pos[,2] <- pos[,2] - 1
    jet_num <- jet_num %% length(jets) + 1
    if(any(mat[cbind((pos[,1] + 1), pos[,2])] == 1)) break
    pos[,1] <- pos[,1] + 1
  }
  
  mat[pos] <- 1
  mat <- mat[apply(mat,1,sum)!=0,] |> unname()
  heights[rock_num] <- nrow(mat)
  rock_num <- rock_num + 1
  rock_type <- (rock_type %% 5) + 1
}
```

### part 2 solution

``` r
height_diff <-  (heights - 1) - dplyr::lag(heights - 1,default = 0)
hd <- paste(height_diff,collapse = "")
seq_diff <- stringr::str_locate_all(hd,substr(hd,500,550))[[1]][,1] |> diff() |> unique()

cycles <- (1000000000000 - 500) %/% 1700
cycle_height <- sum(height_diff[seq_len(seq_diff) + 500])

modulo <- (1000000000000 - 500) %% 1700
modulo_height <- sum(height_diff[seq_len(modulo) + 500])

total_height <- cycle_height * cycles + modulo_height + sum(height_diff[1:500])
options(scipen = 99)
total_height
```

    ## [1] 1568823529338
