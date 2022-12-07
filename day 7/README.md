advent of code - day 7
================
chad allison \| 7 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input data

``` r
input = readLines("day7_input.txt")

input[1:50]
```

    ##  [1] "$ cd /"             "$ ls"               "dir fts"           
    ##  [4] "dir jnwr"           "dir lrvl"           "dir nzgprvw"       
    ##  [7] "dir snwqjgj"        "16394 tllvcdr.sjl"  "195891 zbdp.gqb"   
    ## [10] "dir zddrb"          "$ cd fts"           "$ ls"              
    ## [13] "dir dlqtffw"        "dir rbfmmjvd"       "254713 wvwhrb.dhh" 
    ## [16] "$ cd dlqtffw"       "$ ls"               "73533 nqbvg.fgd"   
    ## [19] "$ cd .."            "$ cd rbfmmjvd"      "$ ls"              
    ## [22] "290697 zcgrgff.fnf" "$ cd .."            "$ cd .."           
    ## [25] "$ cd jnwr"          "$ ls"               "323577 ghmtnzr"    
    ## [28] "57588 tdcbdpnr"     "dir wbv"            "dir zzbvdcf"       
    ## [31] "$ cd wbv"           "$ ls"               "dir nmdwbnnr"      
    ## [34] "253584 slzdbm.ncn"  "$ cd nmdwbnnr"      "$ ls"              
    ## [37] "208370 scbcsb.pjg"  "$ cd .."            "$ cd .."           
    ## [40] "$ cd zzbvdcf"       "$ ls"               "8052 ssssrhwz"     
    ## [43] "$ cd .."            "$ cd .."            "$ cd lrvl"         
    ## [46] "$ ls"               "dir bqqltcg"        "189288 cwpwh"      
    ## [49] "90813 jhnddzml.lww" "dir pwc"

### converting input into data frame

``` r
df = data.frame(x = input)

head(df)
```

    ##             x
    ## 1      $ cd /
    ## 2        $ ls
    ## 3     dir fts
    ## 4    dir jnwr
    ## 5    dir lrvl
    ## 6 dir nzgprvw

### creating function to determine current directory

``` r
cd = function(path, dir) {
  
  if (!is.na(dir)) {
    
    if (dir == "..") {
      
      return(head(path, -1))
      
    }
    
    return(c(path, paste0(tail(path, 1), "/", dir)))
    
  }
  
  return(path)
  
}
```

### calculating directory sizes

``` r
dir_sizes = df |>
  tidyr::extract(x, "cd_dir", "cd (.*)", remove = F) |>
  mutate(path = c(accumulate(cd_dir, cd))) |>
  unnest(path) |>
  filter(str_detect(x, "\\d")) |>
  separate(x, into = c("size", "file"), sep = " ", convert = T) |>
  group_by(path) |>
  summarise(size = sum(size))

head(dir_sizes)
```

    ## # A tibble: 6 x 2
    ##   path               size
    ##   <chr>             <int>
    ## 1 /              46592386
    ## 2 //fts            618943
    ## 3 //fts/dlqtffw     73533
    ## 4 //fts/rbfmmjvd   290697
    ## 5 //jnwr           851171
    ## 6 //jnwr/wbv       461954

### part 1 solution

``` r
dir_sizes |>
  filter(size < 100000) |>
  summarise(size = sum(size)) |>
  pull(size)
```

    ## [1] 1642503

### part 2 solution

``` r
total_disk_space = 70000000
needed_space = 30000000

total_size = dir_sizes |>
  filter(path == "/") |>
  pull(size)

dir_sizes |>
  filter(size >= needed_space - (total_disk_space - total_size)) |>
  arrange(size) |>
  head(1) |>
  pull(size)
```

    ## [1] 6999588
