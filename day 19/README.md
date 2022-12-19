advent of code - day 19
================
chad allison \| 19 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input data

``` r
input = readLines("input.txt")
input[1:5]
```

    ## [1] "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 17 clay. Each geode robot costs 4 ore and 16 obsidian."
    ## [2] "Blueprint 2: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 20 clay. Each geode robot costs 2 ore and 8 obsidian." 
    ## [3] "Blueprint 3: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 13 clay. Each geode robot costs 3 ore and 15 obsidian."
    ## [4] "Blueprint 4: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 14 clay. Each geode robot costs 3 ore and 17 obsidian."
    ## [5] "Blueprint 5: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 19 clay. Each geode robot costs 2 ore and 20 obsidian."

### formatting input data

``` r
input_clean = tibble(blueprint_id = str_match(input,"Blueprint (\\d+):")[, 2],
                     ore_rob_ore = str_match(input,"ore robot costs (\\d+) ore")[, 2],
                     clay_rob_ore = str_match(input,"clay robot costs (\\d+) ore")[, 2],
                     obs_rob_ore = str_match(input,"obsidian robot costs ((\\d+) ore)?( and)?( (\\d+) clay)?")[, 3],
                     obs_rob_clay = str_match(input,"obsidian robot costs ((\\d+) ore)?( and)?( (\\d+) clay)?")[, 6],
                     geo_rob_ore = str_match(input,"geode robot costs ((\\d+) ore)?( and)?( (\\d+) obsidian)?")[, 3],
                     geo_rob_obs = str_match(input,"geode robot costs ((\\d+) ore)?( and)?( (\\d+) obsidian)?")[, 6]) |>
  mutate(across(everything(), as.numeric))

head(input_clean)
```

    ## # A tibble: 6 × 7
    ##   blueprint_id ore_rob_ore clay_rob_ore obs_rob_ore obs_rob_clay geo_rob_ore
    ##          <dbl>       <dbl>        <dbl>       <dbl>        <dbl>       <dbl>
    ## 1            1           4            4           4           17           4
    ## 2            2           4            4           4           20           2
    ## 3            3           2            3           3           13           3
    ## 4            4           4            4           2           14           3
    ## 5            5           3            3           2           19           2
    ## 6            6           4            3           3           17           3
    ## # … with 1 more variable: geo_rob_obs <dbl>

### creating function for next scenario

``` r
scenario_next = function(scenarios) {
  # build order: ore robot
  # prereq: less ore robots than can use in a turn
  scenarios_ore = scenarios |>
    filter(ore_robots < max_ore_robots) |>
    mutate(eta = pmax(0, ceiling((ore_rob_ore - ore) / ore_robots)) + 1) |>
    mutate(ore  = ore - ore_rob_ore - eta,
           ore_robots = ore_robots + 1)
  
  # build order: clay robot
  scenarios_clay = scenarios |>
    filter(clay_robots < obs_rob_clay) |>
    mutate(eta = pmax(0, ceiling((clay_rob_ore - ore) / ore_robots)) + 1) |>
    mutate(ore  = ore - clay_rob_ore,
           clay = clay - eta,
           clay_robots = clay_robots + 1)
  
  # build order: obsidian robot
  # prereq: clay robot
  scenarios_obs = scenarios |>
    filter(clay_robots > 0) |>
    mutate(eta = pmax(0, ceiling((obs_rob_ore  - ore ) / ore_robots), ceiling((obs_rob_clay - clay) / clay_robots)) + 1) |>
    mutate(ore  = ore  - obs_rob_ore,
           clay = clay - obs_rob_clay,
           obs = obs - eta,
           obs_robots = obs_robots + 1)
  
  # build order: geode robot
  # prereq: obsidian robot
  scenarios_geo = scenarios |>
    filter(obs_robots > 0) |>
    mutate(eta = pmax(0, ceiling((geo_rob_ore  - ore ) / ore_robots), ceiling((geo_rob_obs - obs) / obs_robots)) + 1) |>
    mutate(ore  = ore  - geo_rob_ore,
           obs  = obs  - geo_rob_obs,
           geo = geo - eta,
           geo_robots = geo_robots + 1)
  
  # built order: idle till t = 24
  # prereq: geode robot
  scenarios_idle = scenarios |>
    filter(geo_robots > 0) |>
    mutate(eta = max_time - time)
  
  scenarios = bind_rows(scenarios_ore, scenarios_clay, scenarios_obs, scenarios_geo, scenarios_idle) |>
    mutate(time = time + eta,
           ore  = ore  + eta * ore_robots ,
           clay = clay + eta * clay_robots,
           obs  = obs  + eta * obs_robots ,
           geo  = geo  + eta * geo_robots ) %>%
    select(-eta)
  
  return(scenarios)
}
```

### part 1

``` r
max_time = 24
ans = 0

for (i in 1:30) {
  blueprint = input_clean |> slice(i)
  ore_rob_ore  = blueprint$ore_rob_ore
  clay_rob_ore = blueprint$clay_rob_ore
  obs_rob_ore  = blueprint$obs_rob_ore
  obs_rob_clay = blueprint$obs_rob_clay
  geo_rob_ore  = blueprint$geo_rob_ore
  geo_rob_obs  = blueprint$geo_rob_obs
  max_ore_robots = max(clay_rob_ore, obs_rob_ore, geo_rob_ore)
  geo_max = 0
  
  scenarios = tibble(time = 0, ore = 0, clay = 0, obs = 0, geo = 0, ore_robots = 1, clay_robots = 0, obs_robots = 0, geo_robots = 0)
  
  while (nrow(scenarios > 0)) {
    
    scenarios = scenario_next(scenarios)
    
    geo_max = max(geo_max, scenarios |>
                    filter(time == max_time) |>
                    pull(geo))
    
    scenarios = scenarios |>
      filter(time < max_time)
    
  }
  
  print(paste(i, geo_max))
  ans = ans + i * geo_max
}
```

    ## [1] "1 0"
    ## [1] "2 0"
    ## [1] "3 5"
    ## [1] "4 0"
    ## [1] "5 0"
    ## [1] "6 0"
    ## [1] "7 1"
    ## [1] "8 2"
    ## [1] "9 0"
    ## [1] "10 0"
    ## [1] "11 16"
    ## [1] "12 3"
    ## [1] "13 5"
    ## [1] "14 5"
    ## [1] "15 1"
    ## [1] "16 6"
    ## [1] "17 3"
    ## [1] "18 1"
    ## [1] "19 0"
    ## [1] "20 3"
    ## [1] "21 1"
    ## [1] "22 3"
    ## [1] "23 1"
    ## [1] "24 4"
    ## [1] "25 9"
    ## [1] "26 4"
    ## [1] "27 1"
    ## [1] "28 1"
    ## [1] "29 0"
    ## [1] "30 5"

### part 1 solution

``` r
ans
```

    ## [1] 1365

### part 2

``` r
max_time = 32
ans = 1

for (i in 1:3) {
  blueprint = input_clean |> slice(i)
  ore_rob_ore  = blueprint$ore_rob_ore
  clay_rob_ore = blueprint$clay_rob_ore
  obs_rob_ore  = blueprint$obs_rob_ore
  obs_rob_clay = blueprint$obs_rob_clay
  geo_rob_ore  = blueprint$geo_rob_ore
  geo_rob_obs  = blueprint$geo_rob_obs
  max_ore_robots = max(clay_rob_ore, obs_rob_ore, geo_rob_ore)
  geo_max = 0

  scenarios = tibble(time = 0, ore = 0, clay = 0, obs = 0, geo = 0, ore_robots = 1, clay_robots = 0, obs_robots = 0, geo_robots = 0)

  while (nrow(scenarios > 0)) {

    scenarios = scenario_next(scenarios)

    geo_max = max(geo_max, scenarios |> filter(time == max_time) |> pull(geo))

    # a couple explicit dead end filters
    # any geode robot needs 7+ obsidian, so time >= 27 with 0 obsidian robots is a no-go
    # any obsidian robot needs 5+ clay, so time >= 12 with 0 clay robots is a no-go
    # any scenario where making a geode robot a turn won't get you to the goal anymore is a no-go
    scenarios = scenarios |>
      filter(time < max_time) |>
      filter(!(time >= max_time - 7 & obs_robots == 0)) |>
      filter(!(time >= max_time - 12 & clay_robots == 0)) |>
      filter((max_time - time) * geo_robots + (max_time - time) * (max_time-time + 1) / 2 + geo > geo_max)

  }
  
  print(paste(i, geo_max))
  ans = ans * geo_max
}
```

    ## [1] "1 8"
    ## [1] "2 16"
    ## [1] "3 38"

### part 2 solution

``` r
ans
```

    ## [1] 4864
