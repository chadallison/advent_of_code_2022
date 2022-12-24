advent of code - day 23
================
chad allison \| 23 december 2022

### loading packages

``` r
library(tidyverse)
options(scipen = 999)
```

### loading input data

``` r
input_file = readLines("input.txt")
input_file[1:10]
```

    ##  [1] "...###.....#.#....##....#..###.....#.##.#..####..#.##.....##..#####..##"
    ##  [2] "#..##....#####...#.#.#############...#.######.#.##.##...####.#.#.#.#.##"
    ##  [3] ".#..###..#..#......#..#####.....#.###..##.###.#.##..##...#.#.###.#...##"
    ##  [4] "#.####..#..#.##...####.##.#..##..#.#.#..#..#.......#....###...######.##"
    ##  [5] "..###....#...###..###...##..##.##..#.#.......#.#...########..#.##...#.#"
    ##  [6] "##.....#.#....##.......#..#.#.#.##..###..#.##.##.##....#..#........#..."
    ##  [7] "#.#...####...#.#.##.#.##....#####.#########.####..#.#.######...#......."
    ##  [8] "###..##..#.#..#..##.#..#..##.#.............#...##.#..##..###....#.##.#."
    ##  [9] "##.##....#..#.##.....#..#.#.#..###.#.#..##.#.##....##.#...##########..."
    ## [10] "...#...####.#...#...#.#....##..#.....#.#...##..##.#.#...#..##.#..###..#"

### part 1

``` r
input = read_file(input_file)
```

    ## Warning in is.character(x) && file.exists(x): 'length(x) = 71 > 1' in coercion
    ## to 'logical(1)'

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

elfs = read_as_grid(input) |>
  filter(value == "#") |>
  mutate(y = -y)

dirs = as_tibble(data.table::fread("
  f,dir,dx,dy,mx,my
  n,nw,-1,1,0,1
  n,n,0,1,0,1
  n,ne,1,1,0,1
  e,ne,1,1,1,0
  e,e,1,0,1,0
  e,se,1,-1,1,0
  s,se,1,-1,0,-1
  s,s,0,-1,0,-1
  s,sw,-1,-1,0,-1
  w,sw,-1,-1,-1,0
  w,w,-1,0,-1,0
  w,nw,-1,1,-1,0"))

make_move = function(elfs, face_preference) {
  elfs_w_neighbors = elfs |>
    inner_join(dirs, by = character()) |>
    mutate(nx = x + dx, ny = y + dy) |>
    left_join(elfs, by = c("nx" = "x", "ny" = "y")) |>
    rename(value = value.x, nb_value = value.y) |>
    mutate(has_elf = !is.na(nb_value)) |>
    select(x, y, f, has_elf, mx, my)

  happy_in_place = elfs_w_neighbors |>
    group_by(x, y) |>
    summarize(is_free = !any(has_elf), .groups = "drop") |>
    filter(is_free) |>
    select(x, y)
  
  cannot_move = elfs_w_neighbors |>
    group_by(x, y, f, mx, my) |>
    summarize(is_free = !any(has_elf), .groups = "drop") |>
    group_by(x, y) |>
    summarize(can_move = any(is_free), .groups = "drop") |>
    filter(!can_move) |>
    select(x, y) |>
    anti_join(happy_in_place, by = c("x", "y"))
  
  moving = elfs_w_neighbors |>
    anti_join(happy_in_place, by = c("x", "y")) |>
    group_by(x, y, f, mx, my) |>
    summarize(is_free = !any(has_elf), .groups = "drop") |>
    filter(is_free) |>
    mutate(preference = match(f, face_preference)) |>
    group_by(x, y) |>
    summarize(mx = first(mx, order_by = preference),
              my = first(my, order_by = preference),
              .groups = "drop") |>
    mutate(tx = x + mx, ty = y + my) |>
    group_by(tx, ty) |>
    mutate(n_elf = n()) |>
    ungroup() |>
    mutate(x = if_else(n_elf == 1, tx, x), y = if_else(n_elf == 1, ty, y)) |>
    select(x, y) |>
    anti_join(happy_in_place, by = c("x", "y"))
  
  rbind(happy_in_place, moving) |>
    rbind(cannot_move) |>
    mutate(value = "#") |>
    arrange(x, y)
}

rotate = function(face_preference) {
  lead(face_preference, default = face_preference[1])
}

face_preference = c("n", "s", "w", "e")

for (i in seq_len(10)) {
  elfs = make_move(elfs, face_preference)
  ggplot(elfs, aes(x, y)) + geom_tile() + coord_fixed()
  face_preference = rotate(face_preference)
}

head(elfs)
```

    ## # A tibble: 6 × 3
    ##       x     y value
    ##   <int> <int> <chr>
    ## 1    -4   -41 #    
    ## 2    -3   -46 #    
    ## 3    -3   -43 #    
    ## 4    -3   -38 #    
    ## 5    -3   -33 #    
    ## 6    -3   -23 #

### part 1 solution

``` r
p1 = summarize(elfs, v = (max(x) - min(x) + 1) * (max(y) - min(y) + 1) - nrow(elfs)) |>
  pull(v)

p1
```

    ## [1] 3920

# part 2

``` r
i = 1

repeat {
  new_elfs = make_move(elfs, face_preference)
  if (isTRUE(all.equal(elfs, new_elfs))) break
  elfs = new_elfs
  face_preference = rotate(face_preference)
  if (i %% 25 == 0) cat(i, "\n")
  i = i + 1
}
```

    ## 25 
    ## 50 
    ## 75 
    ## 100 
    ## 125 
    ## 150 
    ## 175 
    ## 200 
    ## 225 
    ## 250 
    ## 275 
    ## 300 
    ## 325 
    ## 350 
    ## 375 
    ## 400 
    ## 425 
    ## 450 
    ## 475 
    ## 500 
    ## 525 
    ## 550 
    ## 575 
    ## 600 
    ## 625 
    ## 650 
    ## 675 
    ## 700 
    ## 725 
    ## 750 
    ## 775 
    ## 800 
    ## 825 
    ## 850 
    ## 875

### part 2 solution

``` r
i
```

    ## [1] 879
