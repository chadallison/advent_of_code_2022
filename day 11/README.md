advent of code - day 11
================
chad allison \| 11 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input data

``` r
input = readLines("day11_input.txt")
input[1:28]
```

    ##  [1] "Monkey 0:"                                       
    ##  [2] "  Starting items: 64, 89, 65, 95"                
    ##  [3] "  Operation: new = old * 7"                      
    ##  [4] "  Test: divisible by 3"                          
    ##  [5] "    If true: throw to monkey 4"                  
    ##  [6] "    If false: throw to monkey 1"                 
    ##  [7] ""                                                
    ##  [8] "Monkey 1:"                                       
    ##  [9] "  Starting items: 76, 66, 74, 87, 70, 56, 51, 66"
    ## [10] "  Operation: new = old + 5"                      
    ## [11] "  Test: divisible by 13"                         
    ## [12] "    If true: throw to monkey 7"                  
    ## [13] "    If false: throw to monkey 3"                 
    ## [14] ""                                                
    ## [15] "Monkey 2:"                                       
    ## [16] "  Starting items: 91, 60, 63"                    
    ## [17] "  Operation: new = old * old"                    
    ## [18] "  Test: divisible by 2"                          
    ## [19] "    If true: throw to monkey 6"                  
    ## [20] "    If false: throw to monkey 5"                 
    ## [21] ""                                                
    ## [22] "Monkey 3:"                                       
    ## [23] "  Starting items: 92, 61, 79, 97, 79"            
    ## [24] "  Operation: new = old + 6"                      
    ## [25] "  Test: divisible by 11"                         
    ## [26] "    If true: throw to monkey 2"                  
    ## [27] "    If false: throw to monkey 6"                 
    ## [28] ""

### formating the input as a data frame

``` r
df = data.frame(x = input)
head(df)
```

    ##                                  x
    ## 1                        Monkey 0:
    ## 2   Starting items: 64, 89, 65, 95
    ## 3         Operation: new = old * 7
    ## 4             Test: divisible by 3
    ## 5       If true: throw to monkey 4
    ## 6      If false: throw to monkey 1

### formatting data

``` r
df = df |>
  mutate(monkey = ifelse(str_detect(x, "Monkey"), parse_number(x), NA),
         start = ifelse(str_detect(x, "Starting items"), str_remove(x, "Starting items:"), NA_real_),
         operation = ifelse(str_detect(x, "Operation"), str_remove(x, "Operation: "), NA),
         test = ifelse(str_detect(x, "Test"), parse_number(x), NA),
         next_true = ifelse(str_detect(x, "If true"), parse_number(x), NA),
         next_false = ifelse(str_detect(x, "If false"), parse_number(x), NA),
         x = NULL) |>
  fill(monkey) |>
  group_by(monkey) |>
  fill(everything(), .direction = "downup") |>
  ungroup() |>
  distinct() |>
  mutate(monkey = monkey + 1,
         next_true = next_true + 1,
         next_false = next_false + 1,
         start = strsplit(start, ",") |>
           map(as.numeric))

head(df)
```

    ## # A tibble: 6 x 6
    ##   monkey start     operation            test next_true next_false
    ##    <dbl> <list>    <chr>               <dbl>     <dbl>      <dbl>
    ## 1      1 <dbl [4]> "  new = old * 7"       3         5          2
    ## 2      2 <dbl [8]> "  new = old + 5"      13         8          4
    ## 3      3 <dbl [3]> "  new = old * old"     2         7          6
    ## 4      4 <dbl [5]> "  new = old + 6"      11         3          7
    ## 5      5 <dbl [2]> "  new = old * 11"      5         2          8
    ## 6      6 <dbl [7]> "  new = old + 8"      17         5          1

### implementing logic for part 1

``` r
do_operation = function(old, operation) {
  eval(parse(text = operation))
  return(new)
}

hold = df$start
insp_count = numeric(nrow(df))
round = 1

while (round <= 20) {
  for (monkey in df$monkey) {
    vals = do_operation(hold[[monkey]], df$operation[[monkey]])
    vals = floor(vals / 3)
    test = (vals %% df$test[[monkey]]) == 0
    next_true = df$next_true[monkey]
    next_false = df$next_false[monkey]
    hold[[next_true]] = c(hold[[next_true]], vals[test])
    hold[[next_false]] = c(hold[[next_false]], vals[!test])
    hold[[monkey]] = numeric()
    insp_count[monkey] = insp_count[monkey] + length(vals)
  }
  round = round + 1
}

insp_count
```

    ## [1] 318 296  35 307  39 337 336  40

### part 1 solution

``` r
sort(insp_count) |>
  tail(2) |>
  prod()
```

    ## [1] 113232

### recreating part 1 logic for part 2

``` r
# we can use the same do_operation function

hold = df$start
insp_count = numeric(nrow(df))
round = 1

while (round <= 10000) {
  for (monkey in df$monkey) {
    vals = do_operation(hold[[monkey]], df$operation[[monkey]])
    test = (vals %% df$test[[monkey]]) == 0
    # modulo of vals by the product of tests
    vals = vals %% 9699690
    next_true = df$next_true[monkey]
    next_false = df$next_false[monkey]
    hold[[next_true]] = c(hold[[next_true]], vals[test])
    hold[[next_false]] = c(hold[[next_false]], vals[!test])
    hold[[monkey]] = numeric()
    insp_count[monkey] = insp_count[monkey] + length(vals)
  }
  round = round + 1
}
```

### part 2 solution

``` r
sort(insp_count) |>
  tail(2) |>
  prod()
```

    ## [1] 29703395016
