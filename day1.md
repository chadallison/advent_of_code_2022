advent of code - day 1
================
chad allison \| 1 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### importing input

``` r
input = readLines("day1_input.txt")
input[1:50]
```

    ##  [1] "62797" ""      "1137"  "6086"  "6104"  "1895"  "7909"  "1651"  "4973" 
    ## [10] "6964"  "5989"  "6003"  "6859"  ""      "2817"  "3841"  "5360"  "2614" 
    ## [19] "1746"  "3507"  "1159"  "3226"  "4541"  "1007"  "3881"  "4243"  "1733" 
    ## [28] "4184"  "2377"  ""      "2528"  "2950"  "5635"  "3664"  "5782"  "4580" 
    ## [37] "4672"  "5989"  "3426"  "5600"  "2057"  "2901"  "6175"  ""      "48610"
    ## [46] ""      "8330"  "9058"  "2953"  "10409"

### formatting the data

``` r
df = data.frame(cals = input) |>
  mutate(elf = cumsum(cals == "") + 1,
         cals = as.numeric(cals)) |>
  filter(!is.na(cals)) |>
  group_by(elf) |>
  summarise(cals = sum(cals)) |>
  arrange(desc(cals))

head(df)
```

    ## # A tibble: 6 x 2
    ##     elf  cals
    ##   <dbl> <dbl>
    ## 1   238 68442
    ## 2   240 68218
    ## 3    27 68177
    ## 4   121 66575
    ## 5   227 66355
    ## 6   105 64717

this tells us the elf with the most calories is elf \#238 carrying
68,442 calories. now we need to find the total calories carried by the
top three elves.

``` r
sum(df[1:3, ]$cals)
```

    ## [1] 204837

the top three elves carrying the most calories are carrying 204,837
calories in total.
