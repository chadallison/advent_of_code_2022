advent of code - day 10
================
chad allison \| 10 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading the input data

``` r
input = readLines("day10_input.txt")
input[1:25]
```

    ##  [1] "noop"     "addx 24"  "addx -19" "noop"     "noop"     "noop"    
    ##  [7] "addx 5"   "noop"     "addx 1"   "addx 5"   "addx -1"  "addx 5"  
    ## [13] "addx 1"   "addx 14"  "addx -9"  "addx -1"  "addx 5"   "noop"    
    ## [19] "addx 2"   "addx -20" "addx 24"  "addx -36" "addx -2"  "noop"    
    ## [25] "addx 3"

### formatting input as data frame

``` r
df = data.frame(input) |>
  mutate(input = ifelse(input == "noop", "noop 0", input)) |>
  separate(input, into = c("command", "value"), sep = " ", convert = T)

head(df)
```

    ##   command value
    ## 1    noop     0
    ## 2    addx    24
    ## 3    addx   -19
    ## 4    noop     0
    ## 5    noop     0
    ## 6    noop     0

### creating a data frame for the cycle time

``` r
cycle_time = data.frame(command = c("noop", "addx"),
                        cycle = c(1, 2))

cycle_time
```

    ##   command cycle
    ## 1    noop     1
    ## 2    addx     2

### creating `id`, `cycle`, `register`, and `signal_strength` variables

``` r
cpu = df |>
  left_join(cycle_time, by = "command") |>
  mutate(id = row_number()) |>
  uncount(cycle) |>
  mutate(cycle = row_number(),
         register = cumsum(ifelse(id != lag(id, default = -1), lag(value, default = 0), 0)) + 1,
         signal_strength = register * cycle)

head(cpu)
```

    ##   command value id cycle register signal_strength
    ## 1    noop     0  1     1        1               1
    ## 2    addx    24  2     2        1               2
    ## 3    addx    24  2     3        1               3
    ## 4    addx   -19  3     4       25             100
    ## 5    addx   -19  3     5       25             125
    ## 6    noop     0  4     6        6              36

### part 1 solution

``` r
cpu |>
  filter(cycle %in% c(20, 60, 100, 140, 180, 220)) |>
  summarise(sum = sum(signal_strength)) |>
  pull(sum)
```

    ## [1] 13440

### creating grid with image content

``` r
crt = cpu |>
  transmute(row = (cycle - 1) %/% 40,
            col = (cycle - 1) %% 40,
            draw = ifelse(col >= register - 1 & col <= register + 1, "#", " "))

head(crt)
```

    ##   row col draw
    ## 1   0   0    #
    ## 2   0   1    #
    ## 3   0   2    #
    ## 4   0   3     
    ## 5   0   4     
    ## 6   0   5    #

### reformatting data

``` r
crt = crt |>
  pivot_wider(names_from = "col", values_from = "draw") |>
  select(-row) |>
  as.matrix() |>
  apply(MARGIN = 1, \(x) paste(x, collapse = ""))

crt
```

    ## [1] "###  ###  ####  ##  ###   ##  ####  ##  "
    ## [2] "#  # #  #    # #  # #  # #  #    # #  # "
    ## [3] "#  # ###    #  #    #  # #  #   #  #  # "
    ## [4] "###  #  #  #   # ## ###  ####  #   #### "
    ## [5] "#    #  # #    #  # # #  #  # #    #  # "
    ## [6] "#    ###  ####  ### #  # #  # #### #  # "

### part 2 solution

``` r
cat(crt, sep = "\n")
```

    ## ###  ###  ####  ##  ###   ##  ####  ##  
    ## #  # #  #    # #  # #  # #  #    # #  # 
    ## #  # ###    #  #    #  # #  #   #  #  # 
    ## ###  #  #  #   # ## ###  ####  #   #### 
    ## #    #  # #    #  # # #  #  # #    #  # 
    ## #    ###  ####  ### #  # #  # #### #  #
