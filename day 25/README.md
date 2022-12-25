advent of code - day 25
================
chad allison \| 25 december 2022

### loading packages

``` r
library(tidyverse)
options(digits = 22)
```

### loading input data

``` r
input = readLines("input.txt")
input[1:25]
```

    ##  [1] "10===1"            "1-2=1-2-===11-1"   "10-0"             
    ##  [4] "2==-10"            "12=10=0--01-0=000" "120-==1"          
    ##  [7] "1-11-===0-"        "2-00=1="           "1-010=1112==1-="  
    ## [10] "2=-=220--2=-=210"  "10-0-2-="          "2=011120"         
    ## [13] "1-0-1-1=0=202"     "1010=2021=0-=="    "11=20212010"      
    ## [16] "1-==1"             "1=1-2==21-"        "1-=="             
    ## [19] "22=2=0=120="       "2-22020-2-2=-22"   "2-022-2-1112212"  
    ## [22] "1==0"              "1222-"             "11==10"           
    ## [25] "2221120-"

### part 1

``` r
tr = cbind(c(2, 1, 0, "-", "="),
           c(5:1) - 3)

tot = sum(sapply(strsplit(input, ""), function(x) {
  sum(sapply(x, function(y) as.integer(tr[tr[, 1] == y, 2])) * 5 ^ ((length(x):1) - 1))
}))
```

### solution as decimal

``` r
tot
```

    ## [1] 33078355623611

### converting decimal to SNAFU

``` r
res = rep("0", 21)
slots = 5 ^ (20:0)
i = 1

while (tot != 0) {
  w = which.min(abs(tot - slots[i] * 2:-2))
  res[i] = tr[w, 1]
  tot = tot - slots[i] * (2:-2)[w]
  i = i + 1
}
```

### part 1 solution

``` r
gsub("^0+","", paste(res, collapse = ""))
```

    ## [1] "2-=2-0=-0-=0200=--21"
