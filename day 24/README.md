advent of code - day 24
================
chad allison \| 24 december 2022

### loading packages

``` r
library(tidyverse)
library(tidygraph)
library(glue)
options(scipen = 999)
```

### loading input data

``` r
input_file = glue("input.txt")
input = read_file(input_file)
substr(input, 1, 10)
```

    ## [1] "#.########"

### part 1

``` r
read_as_grid = function(input, col_sep = "", convert = T) {
  tibble(value = input) |>
    separate_rows(value, sep = "\n", convert = FALSE) |>
    mutate(y = 1:n()) |>
    separate_rows(value, sep = col_sep, convert = convert) |>
    filter(value != "") |>
    group_by(y) |>
    mutate(x = 1:n()) |>
    ungroup()
}

initial_map = read_as_grid(input)
width = max(initial_map$x)
height = max(initial_map$y)
area = crossing(x = 2:(width - 1), y = 2:(height - 1))

dirs = as_tibble(data.table::fread("
  value,dx,dy
  >,1,0
  v,0,1
  <,-1,0
  ^,0,-1
  W,0,0"))

start = initial_map |> filter(value == "." & y == 1) |> select(x, y)
end = initial_map |> filter(value == "." & y == height) |> select(x, y)
start_end = rbind(start, end)
snow = inner_join(initial_map, dirs, by = "value") |> select(x, y, value)
free = anti_join(area, snow, by = c("x", "y")) |> rbind(start_end) |> mutate(t = 1)

advance_snow = function(snow) {
  inner_join(snow, dirs, by = "value") |>
    mutate(x = case_when(x + dx == 1 ~ width - 1L,
                         x + dx == width ~ 2L,
                         T ~ x + dx),
           y = case_when(y + dy == 1 ~ height - 1L,
                         y + dy == height ~ 2L,
                         T ~ y + dy)) |>
    select(value, x, y)
}

for (t in 2:1024) {
  snow = advance_snow(snow)
  free = anti_join(area, snow, by = c("x", "y")) |>
    rbind(start_end) |>
    mutate(t = t) |>
    rbind(free)
}

nodes = free |>
  arrange(t, y, x) |>
  mutate(node_id = row_number(), name = paste(x, y, t, sep = ";"))

edges = nodes |>
  inner_join(dirs, by = character()) |>
  mutate(nx = x + dx, ny = y + dy, nt = t + 1) |>
  inner_join(nodes, by = c("nx" = "x", "ny" = "y", "nt" = "t")) |>
  rename(from = node_id.x, to = node_id.y) |>
  select(from, to)

g = tbl_graph(select(nodes, node_id, name), edges)

shortest_path_to_one_of = function(g, start_node_id, end_nodes) {
  g |>
    activate(nodes) |>
    mutate(dist_from_start = node_distance_from(start_node_id)) |>
    activate(nodes) |>
    as_tibble() |>
    inner_join(end_nodes, by = "node_id") |>
    pull(dist_from_start) |>
    min()
}

# part 1 ------------------

first_there_min = shortest_path_to_one_of(
  g,
  inner_join(nodes, mutate(start, t = 1), by = c("x", "y", "t"))$node_id,
  inner_join(nodes, end, by = c("x", "y")) |> select(t, node_id))
```

### part 1 solution

``` r
first_there_min
```

    ## [1] 221

### part 2

``` r
back_min = shortest_path_to_one_of(
  g,
  inner_join(nodes, mutate(end, t = first_there_min + 1), by = c("x", "y", "t"))$node_id,
  inner_join(nodes, start, by = c("x", "y")) |> select(t, node_id))

second_there_min = shortest_path_to_one_of(
  g,
  inner_join(nodes, mutate(start, t = first_there_min + back_min + 1), by = c("x", "y", "t"))$node_id,
  inner_join(nodes, end, by = c("x", "y")) |> select(t, node_id))
```

### part 2 solution

``` r
first_there_min + back_min + second_there_min
```

    ## [1] 739
