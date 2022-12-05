advent of code - day 5
================
chad allison \| 5 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading the input data

``` r
input = readLines("day5_input.txt")

input[1:30]
```

    ##  [1] "    [S] [C]         [Z]            " "[F] [J] [P]         [T]     [N]    "
    ##  [3] "[G] [H] [G] [Q]     [G]     [D]    " "[V] [V] [D] [G] [F] [D]     [V]    "
    ##  [5] "[R] [B] [F] [N] [N] [Q] [L] [S]    " "[J] [M] [M] [P] [H] [V] [B] [B] [D]"
    ##  [7] "[L] [P] [H] [D] [L] [F] [D] [J] [L]" "[D] [T] [V] [M] [J] [N] [F] [M] [G]"
    ##  [9] " 1   2   3   4   5   6   7   8   9 " ""                                   
    ## [11] "move 3 from 4 to 6"                  "move 1 from 5 to 8"                 
    ## [13] "move 3 from 7 to 3"                  "move 4 from 5 to 7"                 
    ## [15] "move 1 from 7 to 8"                  "move 3 from 9 to 4"                 
    ## [17] "move 2 from 8 to 2"                  "move 4 from 4 to 5"                 
    ## [19] "move 2 from 5 to 1"                  "move 2 from 5 to 6"                 
    ## [21] "move 7 from 8 to 1"                  "move 9 from 3 to 9"                 
    ## [23] "move 11 from 6 to 5"                 "move 2 from 6 to 7"                 
    ## [25] "move 12 from 1 to 4"                 "move 10 from 2 to 9"                
    ## [27] "move 2 from 3 to 9"                  "move 1 from 7 to 5"                 
    ## [29] "move 4 from 7 to 6"                  "move 2 from 6 to 1"

### splitting the input into two data frames

``` r
moves = data.frame(moves = input[11:length(input)])

stacks = input[1:8] |>
  paste(collapse = "\n") |>
  read_fwf(show_col_types = F) |>
  set_names(paste0("s", 1:9))

head(moves)
```

    ##                moves
    ## 1 move 3 from 4 to 6
    ## 2 move 1 from 5 to 8
    ## 3 move 3 from 7 to 3
    ## 4 move 4 from 5 to 7
    ## 5 move 1 from 7 to 8
    ## 6 move 3 from 9 to 4

``` r
stacks
```

    ## # A tibble: 8 x 9
    ##   s1    s2    s3    s4    s5    s6    s7    s8    s9   
    ##   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    ## 1 <NA>  [S]   [C]   <NA>  <NA>  [Z]   <NA>  <NA>  <NA> 
    ## 2 [F]   [J]   [P]   <NA>  <NA>  [T]   <NA>  [N]   <NA> 
    ## 3 [G]   [H]   [G]   [Q]   <NA>  [G]   <NA>  [D]   <NA> 
    ## 4 [V]   [V]   [D]   [G]   [F]   [D]   <NA>  [V]   <NA> 
    ## 5 [R]   [B]   [F]   [N]   [N]   [Q]   [L]   [S]   <NA> 
    ## 6 [J]   [M]   [M]   [P]   [H]   [V]   [B]   [B]   [D]  
    ## 7 [L]   [P]   [H]   [D]   [L]   [F]   [D]   [J]   [L]  
    ## 8 [D]   [T]   [V]   [M]   [J]   [N]   [F]   [M]   [G]

### reformatting the `moves` data frame

``` r
moves = moves |>
  separate(moves, into = c("move", "from"), sep = "from") |>
  separate(from, into = c("from", "to"), sep = "to") |>
  mutate(move = str_remove(move, "move"),
         move = as.numeric(move),
         from = as.numeric(from),
         to = as.numeric(to))

head(moves)
```

    ##   move from to
    ## 1    3    4  6
    ## 2    1    5  8
    ## 3    3    7  3
    ## 4    4    5  7
    ## 5    1    7  8
    ## 6    3    9  4

### converting the stacks into lists

``` r
stacks_list = stacks |>
  as.list(col) |>
  map(na.omit)

head(stacks_list[[1]])
```

    ## [1] "[F]" "[G]" "[V]" "[R]" "[J]" "[L]"

### creating function for making a move

``` r
make_move = function(sl, move, from, to) {
  crates = head(sl[[from]], move)
  sl[[from]] = tail(sl[[from]], -move)
  sl[[to]] = c(rev(crates), sl[[to]])
  
  sl
}

sl = stacks_list
```

### applying function to data

``` r
for (i in 1:nrow(moves)) {
  sl = make_move(sl, moves$move[i], moves$from[i], moves$to[i])
}

sl
```

    ## $s1
    ## [1] "[Q]" "[G]"
    ## 
    ## $s2
    ## [1] "[M]" "[V]"
    ## 
    ## $s3
    ## [1] "[B]" "[N]"
    ## 
    ## $s4
    ## [1] "[M]"
    ## 
    ## $s5
    ##  [1] "[J]" "[V]" "[G]" "[L]" "[R]" "[J]" "[J]" "[L]" "[G]" "[S]" "[D]" "[D]"
    ## [13] "[H]" "[L]" "[F]" "[D]" "[P]" "[H]" "[V]" "[F]" "[V]" "[D]" "[G]" "[N]"
    ## [25] "[Z]" "[V]" "[S]" "[F]" "[B]" "[T]" "[M]" "[H]" "[J]"
    ## 
    ## $s6
    ## [1] "[D]" "[N]" "[N]" "[D]"
    ## 
    ## $s7
    ## [1] "[F]"
    ## 
    ## $s8
    ## [1] "[T]"
    ## 
    ## $s9
    ##  [1] "[D]" "[P]" "[L]" "[M]" "[C]" "[B]" "[F]" "[P]" "[G]" "[Q]"

### part 1 solution

``` r
top_stacks = ""

for (i in 1:9) {
  top_stacks = paste0(top_stacks, str_remove_all(sl[[i]][1], "\\[|\\]"))
}

top_stacks
```

    ## [1] "QMBMJDFTD"

### creating function for part 2

``` r
make_move2 = function (sl, move, from, to) {
  crates = head(sl[[from]], move)
  sl[[from]] = tail(sl[[from]], -move)
  sl[[to]] = c(crates, sl[[to]])
  
  sl
}

sl = stacks_list
```

### applying the new function to the data

``` r
for (i in 1:nrow(moves)) {
  sl = make_move2(sl, moves$move[i], moves$from[i], moves$to[i])
}
```

### part 2 solution

``` r
top_stacks = ""

for (i in 1:9) {
  top_stacks = paste0(top_stacks, str_remove_all(sl[[i]][1], "\\[|\\]"))
}

top_stacks
```

    ## [1] "NBTVTJNFJ"
