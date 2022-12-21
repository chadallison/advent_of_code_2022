advent of code - day 21
================
chad allison \| 21 december 2022

### loading packages

``` r
library(tidyverse)
library(tidygraph)
options(scipen = 999)
```

### loading input data

``` r
input = readLines("input.txt")
input[1:25]
```

    ##  [1] "mvnb: mhgv * zvcm" "nrwg: pqnb + wznq" "mcbv: 1"          
    ##  [4] "vwnl: 15"          "ggdz: bgvn + mwjn" "nnmj: snhj * fpmt"
    ##  [7] "zmzd: ttjq * zrhr" "tdpj: nvdr * mlmj" "mdps: jfrh * spnj"
    ## [10] "wmwm: fdcg * pmbd" "qrcq: vrpq + bwpv" "sgzv: 2"          
    ## [13] "zhrt: 5"           "mzwm: lzmq + ldzw" "bwhp: 2"          
    ## [16] "clfd: 4"           "rmwq: 11"          "pqtc: srtc * vjhc"
    ## [19] "cqrh: bnph * jzrj" "mbjq: 5"           "tqdl: rfll + dbhj"
    ## [22] "lsbb: 11"          "fcld: cgjq * qmbc" "mqvz: 6"          
    ## [25] "dghz: 8"

### part 1

``` r
input = tibble(x = input)
parsed = input |>
  extract(x, c("name", "yell"), regex = "(.*): (.*)") |>
  mutate(
    node_id = row_number(),
    expr = paste(name, yell, sep = " <- "))

nodes <- select(parsed, node_id, name)
edges <- parsed |>
  filter(is.na(as.integer(yell))) |>
  separate_rows(yell) |>
  select(from = yell, to = name)
```

    ## Warning in mask$eval_all_filter(dots, env_filter): NAs introduced by coercion

``` r
g <- tbl_graph(nodes, edges)

nodes_in_topo_order <- g |>
  activate(nodes) |>
  mutate(topo_order = node_topo_order()) |>
  activate(nodes) |>
  as_tibble() |>
  arrange(topo_order)

exprs_in_topo_order <- nodes_in_topo_order |>
  left_join(parsed, by = "node_id") |>
  select(topo_order, expr) |>
  arrange(topo_order) |>
  deframe()

for (expr in exprs_in_topo_order) {
  eval(parse(text = expr))
}
root
```

    ## [1] 232974643455000

### part 2

``` r
parsed <- input |>
  extract(x, c("name", "yell"), regex = "(.*): (.*)") |>
  mutate(
    node_id = row_number(),
    expr = paste(name, yell, sep = " <- ")
  ) |>
  mutate(yell = case_when(
    name == "humn" ~ "humn",
    name == "root" ~ sub("\\+", "-", yell),
    TRUE ~ yell
  ))

nodes <- select(parsed, node_id, name)
edges <- parsed |>
  filter(is.na(as.integer(yell))) |>
  separate_rows(yell) |>
  select(from = name, to = yell)

g <- tbl_graph(nodes, edges)

nodes_in_topo_order <- g |>
  activate(nodes) |>
  mutate(topo_order = node_topo_order()) |>
  activate(nodes) |>
  as_tibble() |>
  arrange(topo_order)

exprs_in_topo_order <- nodes_in_topo_order |>
  left_join(parsed, by = c("node_id", "name")) |>
  arrange(topo_order) |>
  select(name, yell) |>
  deframe()

library(symengine)

vars <- lapply(parsed$name, as.name)
names(vars) <- parsed$name
do.call(use_vars, vars)

expr <- S(exprs_in_topo_order[["root"]])

for (var_name in names(exprs_in_topo_order)) {
  expr <- subs(expr, S(var_name), S(exprs_in_topo_order[[var_name]]))
  expr
}

solve(expr, "humn")
```

    ## VecBasic of length 1
    ## V( 3740214169961 )
