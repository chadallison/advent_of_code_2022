advent of code - day 3
================
chad allison \| 3 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### importing and previewing the input data

``` r
input = readLines("day3_input.txt")
df = data.frame(items = input)

head(df)
```

    ##                                            items
    ## 1                         gfWpjRRQffQGCHHJsGqjsj
    ## 2                 SclzJZZvmmnPbJtVSqqNBqVCBdSCsd
    ## 3         tlbvZJDZtmtPcJmlPnhMFQWWpMRFTfLDRRTWRp
    ## 4 HjMPgSWjVrjgbHRRSSMRgjRdpdbGdlcdCvQfcCdlwQJfdf
    ## 5         LNDnhtNtLNFFZDtFnhzvdldDflvvDCdlJfldpJ
    ## 6                     ZFLFZZmFtFtTNTSPRrVPWWMpRP

### splitting the data into the two compartments

``` r
df = df |>
  mutate(comp1 = substr(items, 1, nchar(items) / 2),
         comp2 = substr(items, (nchar(items) / 2 + 1), nchar(items))) |>
  select(comp1, comp2)

head(df)
```

    ##                     comp1                   comp2
    ## 1             gfWpjRRQffQ             GCHHJsGqjsj
    ## 2         SclzJZZvmmnPbJt         VSqqNBqVCBdSCsd
    ## 3     tlbvZJDZtmtPcJmlPnh     MFQWWpMRFTfLDRRTWRp
    ## 4 HjMPgSWjVrjgbHRRSSMRgjR dpdbGdlcdCvQfcCdlwQJfdf
    ## 5     LNDnhtNtLNFFZDtFnhz     vdldDflvvDCdlJfldpJ
    ## 6           ZFLFZZmFtFtTN           TSPRrVPWWMpRP

### identifying common item between compartments

``` r
df$common_item = NA

for (i in 1:nrow(df)) {
  for (j in 1:nchar(df$comp1)[i]) {
    letter = substr(df$comp1[i], j, j)
    df$common_item[i] = ifelse(str_detect(df$comp2[i], letter), letter, df$common_item[i])
  }
}

df = df |>
  select(comp1, comp2, common_item)

head(df)
```

    ##                     comp1                   comp2 common_item
    ## 1             gfWpjRRQffQ             GCHHJsGqjsj           j
    ## 2         SclzJZZvmmnPbJt         VSqqNBqVCBdSCsd           S
    ## 3     tlbvZJDZtmtPcJmlPnh     MFQWWpMRFTfLDRRTWRp           D
    ## 4 HjMPgSWjVrjgbHRRSSMRgjR dpdbGdlcdCvQfcCdlwQJfdf           b
    ## 5     LNDnhtNtLNFFZDtFnhz     vdldDflvvDCdlJfldpJ           D
    ## 6           ZFLFZZmFtFtTN           TSPRrVPWWMpRP           T

### creating data frame for items & priorities

``` r
alphabet_lower = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                   "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

alphabet_upper = str_to_upper(alphabet_lower)

item_priorities = rbind(data.frame(item = alphabet_lower), data.frame(item = alphabet_upper)) |>
  mutate(priority = 1:52)

head(item_priorities)
```

    ##   item priority
    ## 1    a        1
    ## 2    b        2
    ## 3    c        3
    ## 4    d        4
    ## 5    e        5
    ## 6    f        6

### joining together data frames to get item values

``` r
df = df |>
  left_join(item_priorities, by = c("common_item" = "item"))

head(df)
```

    ##                     comp1                   comp2 common_item priority
    ## 1             gfWpjRRQffQ             GCHHJsGqjsj           j       10
    ## 2         SclzJZZvmmnPbJt         VSqqNBqVCBdSCsd           S       45
    ## 3     tlbvZJDZtmtPcJmlPnh     MFQWWpMRFTfLDRRTWRp           D       30
    ## 4 HjMPgSWjVrjgbHRRSSMRgjR dpdbGdlcdCvQfcCdlwQJfdf           b        2
    ## 5     LNDnhtNtLNFFZDtFnhz     vdldDflvvDCdlJfldpJ           D       30
    ## 6           ZFLFZZmFtFtTN           TSPRrVPWWMpRP           T       46

### part 1 solution

``` r
sum(df$priority)
```

    ## [1] 7845

### creating elf groups

``` r
groups_df = data.frame(group = NULL)

for (i in 1:100) {
  group = data.frame(group = rep(i, times = 3))
  groups_df = rbind(groups_df, group)
}

group_nums = groups_df |>
  pull(group)

df = df |>
  mutate(items = paste0(comp1, comp2),
         group = group_nums) |>
  select(items, group)

head(df)
```

    ##                                            items group
    ## 1                         gfWpjRRQffQGCHHJsGqjsj     1
    ## 2                 SclzJZZvmmnPbJtVSqqNBqVCBdSCsd     1
    ## 3         tlbvZJDZtmtPcJmlPnhMFQWWpMRFTfLDRRTWRp     1
    ## 4 HjMPgSWjVrjgbHRRSSMRgjRdpdbGdlcdCvQfcCdlwQJfdf     2
    ## 5         LNDnhtNtLNFFZDtFnhzvdldDflvvDCdlJfldpJ     2
    ## 6                     ZFLFZZmFtFtTNTSPRrVPWWMpRP     2

### finding the common item and its value between each of the groups

``` r
groups_priority = data.frame(group = 1:100, item = NA, priority = NA)

for (i in 1:100) {
  
  data = df |>
    filter(group == i)
  
  common_item = item_priorities |>
    filter(item %in% str_split(data$items[1], pattern = "")[[1]] &
           item %in% str_split(data$items[2], pattern = "")[[1]] &
           item %in% str_split(data$items[3], pattern = "")[[1]])
  
  item = common_item |>
    pull(item)
  
  priority = common_item |>
    pull(priority)

  groups_priority$item[i] = item
  groups_priority$priority[i] = priority
}

head(groups_priority)
```

    ##   group item priority
    ## 1     1    J       36
    ## 2     2    p       16
    ## 3     3    C       29
    ## 4     4    c        3
    ## 5     5    t       20
    ## 6     6    l       12

### part 2 solution

``` r
sum(groups_priority$priority)
```

    ## [1] 2790
