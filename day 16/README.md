advent of code - day 16
================
chad allison \| 16 december 2022

### loading packages

``` r
library(tidyverse)
library(dtplyr)
```

### loading input data

``` r
input = readLines("day16_input.txt")

input[1:10]
```

    ##  [1] "Valve QE has flow rate=3; tunnels lead to valves OU, ME, UX, AX, TW" 
    ##  [2] "Valve TN has flow rate=16; tunnels lead to valves UW, CG, WB"        
    ##  [3] "Valve UX has flow rate=0; tunnels lead to valves AA, QE"             
    ##  [4] "Valve HK has flow rate=5; tunnels lead to valves HT, QU, TW, WV, OK" 
    ##  [5] "Valve SK has flow rate=14; tunnels lead to valves GH, GA, XM"        
    ##  [6] "Valve HY has flow rate=0; tunnels lead to valves LG, AA"             
    ##  [7] "Valve BK has flow rate=0; tunnels lead to valves SZ, AA"             
    ##  [8] "Valve BY has flow rate=11; tunnels lead to valves SP, HS, DN, KD, TK"
    ##  [9] "Valve GR has flow rate=0; tunnels lead to valves FE, OK"             
    ## [10] "Valve OH has flow rate=0; tunnels lead to valves BM, KE"

### formatting input

``` r
input = input |>
  str_replace_all("Valve |has flow rate=|;|,|tunnel(s)? lead(s)? to valve(s)? ", "") |>
  str_split(" ")

input[1:5]
```

    ## [[1]]
    ## [1] "QE" "3"  "OU" "ME" "UX" "AX" "TW"
    ## 
    ## [[2]]
    ## [1] "TN" "16" "UW" "CG" "WB"
    ## 
    ## [[3]]
    ## [1] "UX" "0"  "AA" "QE"
    ## 
    ## [[4]]
    ## [1] "HK" "5"  "HT" "QU" "TW" "WV" "OK"
    ## 
    ## [[5]]
    ## [1] "SK" "14" "GH" "GA" "XM"

### getting flow rates

``` r
flow_rates = lapply(input, function(line) {
  tibble(valve = line[1],
         amount = line[2] %>%
           as.numeric)}) |>
  bind_rows() |>
  filter(amount > 0)

head(flow_rates)
```

    ## # A tibble: 6 x 2
    ##   valve amount
    ##   <chr>  <dbl>
    ## 1 QE         3
    ## 2 TN        16
    ## 3 HK         5
    ## 4 SK        14
    ## 5 BY        11
    ## 6 FE         4

### getting list of paths

``` r
path_list = lapply(input, function(line) {
  tibble(from = line[1],
         to = line[-1:-2])}) |>
  bind_rows()

head(path_list)
```

    ## # A tibble: 6 x 2
    ##   from  to   
    ##   <chr> <chr>
    ## 1 QE    OU   
    ## 2 QE    ME   
    ## 3 QE    UX   
    ## 4 QE    AX   
    ## 5 QE    TW   
    ## 6 TN    UW

### setting current position

``` r
current_position = tibble(curr_pos = "AA",
                          valves_open = "",
                          steam_per_turn = 0,
                          steam_till_now = 0)

current_position
```

    ## # A tibble: 1 x 4
    ##   curr_pos valves_open steam_per_turn steam_till_now
    ##   <chr>    <chr>                <dbl>          <dbl>
    ## 1 AA       ""                       0              0

### part 1 logic

``` r
for (i in 1:30) {
  
  current_position = rbind(current_position |>
                             left_join(path_list, by = c("curr_pos" = "from")) |>
                             mutate(curr_pos = to, to = NULL),
                           current_position |>
                             filter(!str_detect(valves_open, curr_pos)) |>
                             mutate(valves_open = paste0(valves_open, " ", curr_pos)) |>
                             inner_join(flow_rates, by = c("curr_pos" = "valve")) |>
                             mutate(steam_per_turn = steam_per_turn + amount,
                                    steam_till_now = steam_till_now - amount,
                                    amount = NULL)) |>
    mutate(steam_till_now = steam_till_now + steam_per_turn) |>
    lazy_dt() |>
    group_by(curr_pos, valves_open, steam_per_turn) |>
    summarise(steam_till_now = max(steam_till_now),
              .groups = "drop") |>
    collect()
  
  if (i > 20) {
    
    too_low = max(current_position$steam_till_now) - (31 - i) * 100
    
    current_position = current_position |>
      filter(steam_till_now > too_low)
  
  }
  
  print(paste0(i, " ", nrow(current_position)))
  
}
```

    ## [1] "1 5"
    ## [1] "2 6"
    ## [1] "3 18"
    ## [1] "4 36"
    ## [1] "5 61"
    ## [1] "6 117"
    ## [1] "7 205"
    ## [1] "8 333"
    ## [1] "9 541"
    ## [1] "10 883"
    ## [1] "11 1378"
    ## [1] "12 2150"
    ## [1] "13 3346"
    ## [1] "14 5206"
    ## [1] "15 7978"
    ## [1] "16 12222"
    ## [1] "17 18692"
    ## [1] "18 28414"
    ## [1] "19 43008"
    ## [1] "20 64721"
    ## [1] "21 97044"
    ## [1] "22 143697"
    ## [1] "23 169978"
    ## [1] "24 138976"
    ## [1] "25 84869"
    ## [1] "26 35247"
    ## [1] "27 9175"
    ## [1] "28 1578"
    ## [1] "29 349"
    ## [1] "30 56"

### part 1 solution

``` r
max(current_position$steam_till_now)
```

    ## [1] 2077

### setting current position for part 2

``` r
current_position = tibble(curr_pos_1 = "AA",
                          curr_pos_2 = "AA",
                          valves_open = "",
                          steam_per_turn = 0,
                          steam_till_now = 0)

current_position
```

    ## # A tibble: 1 x 5
    ##   curr_pos_1 curr_pos_2 valves_open steam_per_turn steam_till_now
    ##   <chr>      <chr>      <chr>                <dbl>          <dbl>
    ## 1 AA         AA         ""                       0              0

### part 2 logic

``` r
for (i in 1:26) {
  
  current_position = rbind(current_position |>
                             left_join(path_list, by = c("curr_pos_1" = "from")) |>
                             mutate(curr_pos_1 = to, to = NULL),
                           current_position |>
                             filter(!str_detect(valves_open, curr_pos_1)) |>
                             mutate(valves_open = paste0(valves_open, " ", curr_pos_1)) |>
                             inner_join(flow_rates, by = c("curr_pos_1" = "valve")) |>
                             mutate(steam_per_turn = steam_per_turn + amount,
                                    steam_till_now = steam_till_now - amount,
                                    amount = NULL))
  
  current_position = rbind(current_position |>
                             left_join(path_list, by = c("curr_pos_2" = "from")) |>
                             mutate(curr_pos_2 = to, to = NULL),
                           current_position |>
                             filter(!str_detect(valves_open, curr_pos_2)) |>
                             mutate(valves_open = paste0(valves_open, " ", curr_pos_2)) |>
                             inner_join(flow_rates, by = c("curr_pos_2" = "valve")) |>
                             mutate(steam_per_turn = steam_per_turn + amount,
                                    steam_till_now = steam_till_now - amount,
                                    amount = NULL))
  
  current_position = current_position |>
    mutate(tmp = if_else(curr_pos_1 > curr_pos_2, curr_pos_2, curr_pos_1),
           curr_pos_2 = if_else(curr_pos_1 > curr_pos_2, curr_pos_1, curr_pos_2),
           curr_pos_1 = tmp,
           tmp = NULL)
  
  current_position = current_position |>
    mutate(steam_till_now = steam_till_now + steam_per_turn) |>
    lazy_dt() |>
    group_by(curr_pos_1, curr_pos_2, valves_open, steam_per_turn) |>
    summarise(steam_till_now = max(steam_till_now),
              .groups = "drop") |>
    collect()
  
  if (i > 10) {
    
    too_low = max(current_position$steam_till_now) - (27 - i) * 50
    
    current_position = current_position |>
      filter(steam_till_now > too_low)
    
  }
  
  current_position = current_position |>
    arrange(desc(steam_till_now)) |>
    slice(1:max(100, nrow(current_position) * 0.5))
  
  print(paste0(i, " ", nrow(current_position)))
  
}
```

    ## [1] "1 15"
    ## [1] "2 21"
    ## [1] "3 100"
    ## [1] "4 278"
    ## [1] "5 483"
    ## [1] "6 861"
    ## [1] "7 1498"
    ## [1] "8 2563"
    ## [1] "9 3606"
    ## [1] "10 6870"
    ## [1] "11 8967"
    ## [1] "12 15141"
    ## [1] "13 20525"
    ## [1] "14 27292"
    ## [1] "15 41840"
    ## [1] "16 52688"
    ## [1] "17 80789"
    ## [1] "18 111622"
    ## [1] "19 146119"
    ## [1] "20 197704"
    ## [1] "21 245611"
    ## [1] "22 244472"
    ## [1] "23 67727"
    ## [1] "24 25012"
    ## [1] "25 8577"
    ## [1] "26 3326"

### part 2 solution

``` r
max(current_position$steam_till_now)
```

    ## [1] 2741
