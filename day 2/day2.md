advent of code - day 2
================
chad allison \| 2 december 2022

### loading in tidyverse

``` r
library(tidyverse)
```

### loading in input file

``` r
input = readLines("day2_input.txt")
```

### previewing input file

``` r
input[1:100]
```

    ##   [1] "A Y" "A Y" "B X" "A Y" "C Y" "B Y" "B Z" "B Y" "A X" "B X" "C Z" "B Y"
    ##  [13] "B Y" "B Y" "B Y" "C X" "A Z" "B X" "A Y" "A Y" "B Y" "A X" "C X" "B Y"
    ##  [25] "A Y" "B Y" "C X" "A Z" "B Y" "A X" "A Y" "B Y" "A Y" "A X" "B Y" "B Y"
    ##  [37] "B Y" "B X" "A X" "C Y" "A X" "C Z" "B Z" "A Z" "A Z" "C X" "A X" "A X"
    ##  [49] "B Y" "B Y" "C Y" "B Y" "B X" "B Y" "B Y" "B X" "C X" "A X" "A X" "A Z"
    ##  [61] "C X" "A Z" "A X" "B Y" "B Y" "A Y" "A Z" "B Y" "B X" "B Y" "B Y" "B X"
    ##  [73] "C Z" "A X" "B X" "B X" "A Y" "B X" "C Z" "B Y" "A X" "C Z" "A Y" "B Y"
    ##  [85] "B Y" "A X" "A Z" "B Y" "B Y" "A Z" "A Z" "B Y" "A X" "A Z" "C Y" "A X"
    ##  [97] "B Y" "C X" "A X" "B Y"

### formatting the input into a data frame to manipulate

``` r
df = data.frame(input) |>
  separate(input, into = c("opponent", "me"), sep = " ")

head(df)
```

    ##   opponent me
    ## 1        A  Y
    ## 2        A  Y
    ## 3        B  X
    ## 4        A  Y
    ## 5        C  Y
    ## 6        B  Y

### converting letters to choices for quality of life

``` r
df = df |>
  mutate(opponent_choice = case_when(opponent == "A" ~ "rock",
                                     opponent == "B" ~ "paper",
                                     opponent == "C" ~ "scissors"),
         me_choice = case_when(me == "X" ~ "rock",
                               me == "Y" ~ "paper",
                               me == "Z" ~ "scissors"))

head(df)
```

    ##   opponent me opponent_choice me_choice
    ## 1        A  Y            rock     paper
    ## 2        A  Y            rock     paper
    ## 3        B  X           paper      rock
    ## 4        A  Y            rock     paper
    ## 5        C  Y        scissors     paper
    ## 6        B  Y           paper     paper

### creating winner variable

``` r
df = df |>
  mutate(winner = case_when(opponent_choice == "paper" & me_choice == "paper" ~ "draw",
                            opponent_choice == "paper" & me_choice == "rock" ~ "opponent",
                            opponent_choice == "paper" & me_choice == "scissors" ~ "me",
                            opponent_choice == "rock" & me_choice == "paper" ~ "me",
                            opponent_choice == "rock" & me_choice == "rock" ~ "draw",
                            opponent_choice == "rock" & me_choice == "scissors" ~ "opponent",
                            opponent_choice == "scissors" & me_choice == "paper" ~ "opponent",
                            opponent_choice == "scissors" & me_choice == "rock" ~ "me",
                            opponent_choice == "scissors" & me_choice == "scissors" ~ "draw"))

head(df)
```

    ##   opponent me opponent_choice me_choice   winner
    ## 1        A  Y            rock     paper       me
    ## 2        A  Y            rock     paper       me
    ## 3        B  X           paper      rock opponent
    ## 4        A  Y            rock     paper       me
    ## 5        C  Y        scissors     paper opponent
    ## 6        B  Y           paper     paper     draw

### calculating score

``` r
df = df |>
  mutate(score = 0,
         score = case_when(me_choice == "rock" ~ score + 1,
                           me_choice == "paper" ~ score + 2,
                           me_choice == "scissors" ~ score + 3),
         score = case_when(winner == "draw" ~ score + 3,
                           winner == "me" ~ score + 6,
                           T ~ score))

head(df)
```

    ##   opponent me opponent_choice me_choice   winner score
    ## 1        A  Y            rock     paper       me     8
    ## 2        A  Y            rock     paper       me     8
    ## 3        B  X           paper      rock opponent     1
    ## 4        A  Y            rock     paper       me     8
    ## 5        C  Y        scissors     paper opponent     2
    ## 6        B  Y           paper     paper     draw     5

### part 1 solution

``` r
sum(df$score)
```

    ## [1] 11906

### reverting to older version of data

``` r
df = data.frame(input) |>
  separate(input, into = c("opponent", "outcome"), sep = " ") |>
  mutate(opponent_choice = case_when(opponent == "A" ~ "rock",
                                     opponent == "B" ~ "paper",
                                     opponent == "C" ~ "scissors"))

head(df)
```

    ##   opponent outcome opponent_choice
    ## 1        A       Y            rock
    ## 2        A       Y            rock
    ## 3        B       X           paper
    ## 4        A       Y            rock
    ## 5        C       Y        scissors
    ## 6        B       Y           paper

### creating desired outcome variable

``` r
df = df |>
  mutate(desired_outcome = case_when(outcome == "X" ~ "lose",
                                     outcome == "Y" ~ "draw",
                                     outcome == "Z" ~ "win"))

head(df)
```

    ##   opponent outcome opponent_choice desired_outcome
    ## 1        A       Y            rock            draw
    ## 2        A       Y            rock            draw
    ## 3        B       X           paper            lose
    ## 4        A       Y            rock            draw
    ## 5        C       Y        scissors            draw
    ## 6        B       Y           paper            draw

### deciding what shape we need to choose for desired outcome

``` r
df = df |>
  mutate(me_choice = case_when(desired_outcome == "draw" ~ opponent_choice,
                               desired_outcome == "lose" & opponent_choice == "rock" ~ "scissors",
                               desired_outcome == "lose" & opponent_choice == "paper" ~ "rock",
                               desired_outcome == "lose" & opponent_choice == "scissors" ~ "paper",
                               desired_outcome == "win" & opponent_choice == "rock" ~ "paper",
                               desired_outcome == "win" & opponent_choice == "paper" ~ "scissors",
                               desired_outcome == "win" & opponent_choice == "scissors" ~ "rock"))

head(df)
```

    ##   opponent outcome opponent_choice desired_outcome me_choice
    ## 1        A       Y            rock            draw      rock
    ## 2        A       Y            rock            draw      rock
    ## 3        B       X           paper            lose      rock
    ## 4        A       Y            rock            draw      rock
    ## 5        C       Y        scissors            draw  scissors
    ## 6        B       Y           paper            draw     paper

### recreating winner variable and calculating new total score

``` r
df = df |>
  mutate(winner = case_when(opponent_choice == "paper" & me_choice == "paper" ~ "draw",
                            opponent_choice == "paper" & me_choice == "rock" ~ "opponent",
                            opponent_choice == "paper" & me_choice == "scissors" ~ "me",
                            opponent_choice == "rock" & me_choice == "paper" ~ "me",
                            opponent_choice == "rock" & me_choice == "rock" ~ "draw",
                            opponent_choice == "rock" & me_choice == "scissors" ~ "opponent",
                            opponent_choice == "scissors" & me_choice == "paper" ~ "opponent",
                            opponent_choice == "scissors" & me_choice == "rock" ~ "me",
                            opponent_choice == "scissors" & me_choice == "scissors" ~ "draw"),
         score = 0,
         score = case_when(me_choice == "rock" ~ score + 1,
                           me_choice == "paper" ~ score + 2,
                           me_choice == "scissors" ~ score + 3),
         score = case_when(winner == "draw" ~ score + 3,
                           winner == "me" ~ score + 6,
                           T ~ score))

head(df)
```

    ##   opponent outcome opponent_choice desired_outcome me_choice   winner score
    ## 1        A       Y            rock            draw      rock     draw     4
    ## 2        A       Y            rock            draw      rock     draw     4
    ## 3        B       X           paper            lose      rock opponent     1
    ## 4        A       Y            rock            draw      rock     draw     4
    ## 5        C       Y        scissors            draw  scissors     draw     6
    ## 6        B       Y           paper            draw     paper     draw     5

### part 2 solution

``` r
sum(df$score)
```

    ## [1] 11186
