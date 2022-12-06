advent of code - day 6
================
chad allison \| 6 december 2022

### loading tidyverse

``` r
library(tidyverse)
```

### loading input data

``` r
input = readLines("day6_input.txt")

input
```

    ## [1] "mgwwjddqzqdqsstctjjsdjsdsrsfsmfsfwwltwlwhwnhhlffzddgffwlffbsfshfshhgvvdrrltlzlnzznrrnrsnnhgnnfjnnvpnnbjjnwwrcwrrhlhvlhhmzmqzqrqtqmqpmpwwmssgsrgrgtgmtgmtgtdtvdvmvsvsbvsbvbtthmmftmmdnmddcrcvcrrfjfhhfjhffjllcpllmcctjtrttwmtwmwffrlrqlqzzpddsqdqqgjqgjgngwnncjnnvsnswwbzbtzzflzzqsqbsbvbmbnnjpnpnnpfpmpmnpmmjljtltssqnsqslstswtwswwjddvmmzlzqlzqzqjjlttmtrtbtmtgmtmsttrctrrsqrqvvrzrcrhhlnhllbfbtthrhdhllmwlmlgglgsgmgsmszzprpwpfprfftffpssjzjgzjzddqfqmmwqwvwlvlqqtbtwwrwttmsmppbmmpcmctcnnhssnjncnlcnctcjjrzrwrfwfcwffczztrtsrtstlsssljssmvssjzssrqqrcqqwlqwlwffsflssrrzhzzhrzzdgdppspwplpqptttvddggzszccrrnzzwwdwjddrvvwggpvgpvvhdhqddffrnngcncjcjlcchrrftrrjccrcrqqgcglcgcscmmlzmmtcmcffwfcfrcrggdmggdvvnrvnnphnngzzpdpgpspqqgrrnffmfpmffmgfmmjmzztlljlggljjcnnrqnqpnqndnffnwwbpwpjwjjlslmsmtmtjttsvsggrmmdpmmcjjswsqqwfwwrwffczfzggqvggdlldhllsdsfdsdhhmmzmjjmpjpddsccqrrjhjlhjjcnnpwnnffjwwcsszrrnmnsmnnjbnndwnnnhnwwjtwtlwtwqqbnqnbnqqfjfdjdbbwbqwqpqggbcbhhtrtqrrddpdwdlwdddzvzwvvdfdpdcdvdtdpttwwdzdzmdmqmzmnzmnmhmwmjwwshhcqcpcvvzgzdggnjnnhwnhhswwvccqrqlqggcngnmggmffblbglltlstshhrjjlvlppsqslljtjtgglvltvlvmllhrhdrrmqrmqmjjdcjjppqwwllvsvsrszslsvvghvvhmmfbfvfpfmmvdvppwggtrrjvvsbbzffbmffpqqqhnhncclzczwcwpwssrprfrsrbsbnbvnnwzwqqpsqsspmssztssstrtcczsznzvvpvttnssdjdhjddngdgvvmsszbzsbsmbbgsbgsgmmhwhghrhjhphshchgglmlvlhlbhlldwdggdsscvcbcssfbbvggvwwtstltrrwttjdtdvttlsttfhfmhhcbclbcbffqqslshlldhdqhhjwwlffrbrdbrrgcrrfmffbhhlslrslrslrlsrsnsnvvqfqnnfdfmfmttmcmcrrcmmmjttjvtvvjbjqjnnbtbnblbtlblplgltlqltlztzvvtdvvtpvvwdwfflbflfrrhbrbbmjmcjmccztzwwjzwwzwdzdnnwcclbllqgghjhlhthwrdglrmcpbmtrnrdtvjrpmzqmljzzrtpzsrhnjrsdmpnsgdhvqchcfqjqdncjqfnscwjqvszpzzfhpjljmvsqnjzmrsgsbzlvrddtdmwbwwgprlvdfflrpztdzrhtmlzrrtdmpmcprqzzwlnmfjvsrltfjgcnnfllnzmbjcbthvbffczsspmczrpgpdjmvrvfmprfmnqdcnfwwvgdrwvrbtlqmhrrjvtrmmgrlprtnzdlszgbtbwztdrmpmlfblshzcnsczlblgwzrpnlccwhmcqhssmpznbdnnqgzzmjprjttdjhmjbmgqvzblsjwmplzsthrswhsdbvtqgrfzmbpqtpqgqdqcvzlgjrtvrhvzgmcmrwdmfpdvjddsmmsnvrdgnsbsdzcbprbqchqcgnwmfsrmqtrcdhdtzztbvmpblftwqlmlmmjcjhhjlgnnhljnncvbnjhgbjrltlwscswgvqmcnssbcdrtbgnhgmpmvjwtrbrbrdbdqfrncvhdstwztwcpbjrjwzmdlwvlvmsrhghjwjnjstbcqjqtjrgcvhzjdhdgbgdlhvjmztwvhgzzggwwhhhzvtrldchztmwfjvnqnvhnwpfvzzvnlvsccmvsngzgtnttssmdmhwzlhtpnfhczsdfnrstbwvwpqmslcvpvhfzttzhsgzpbhqdtswshljpncznjhzmgvvbcllmzprhrvwljwcjpcdqmwbzvsdcgtmwnrhswsgqhwpwhbjpnhnpjvgsqcjltzrqvqfflcdcvpwnznvtqbfbtlpmtdgbbwdwncqsqnbtgfdzzqzzvjnwmzdmlgstmnjwznjqghglvmwjzlqrnddcqhgndlhlbmqdhrqgrjqztnhpzssnwmrqclmwpgbvfrvgqqvtthznsqwgndjrprbgrhcvhpzbfhdmgnhsrqjvjstbtmnltsbjfzczvjqnhtldqclsflbhvvlzjwrqqgbgpwqwpfjctqpzdqwcfstmwbzgrgrtzngljjnvtggrqcbgjwtqsdgwmfjqppnzgfsfdmlctztbhnntnntdlvrsdvnllvmpggjzspqfhzwrttwzpqrnqjhmpjnmrzrpnqzshcqgctbtflqflcrzpmnphgbbghhwzplljwngbtffwmrwggdztvtfgwldlswqvjptvbfvnbpglhgrdgcfmvrslqldmwjqvjpvwgpjddvglllvpqwvbchqsmjrncgvgmqbsbcwfbsbpqcqzjfpcdzszgmvqgqjlflpfzbsrhsrzrdbpssrjbcfhvztftlzqpsglpwhbscgwdlbgghzsbwznnbgnnsgjghmmpmmrmqmdhnflgvgprqfcbpzbcpjscvnpfrmtvzsbflmffvcfsvdsggzdqtppcjzphcqwrqtrczqmwcdmdqndzmhdpnfqsbndnvjlzrsjzmpcrfgjwccsdtzvslccwhlvzjwjgvwpsnsggmqgsjfbwmjstsgnqmtjhljvfnflnngdrqvscwlqqdsglhghczhjdvgrjcqblmncdbjvsbwgptgpvvzhcjgjnvttrgzrjnqlvfbrmpzdcbbnnrqptpzpssznbsrstdphbgdrsnrhcjwwgsncdzvqfnmnvqcmcgdgjdbqjzdrvvbvhjdfcqndmqwscmsvppclzrhgbldqtwctbdhpbbwfvwpcpsvddmrhqbhlrrmrblnmqqqbwvcwwbwprlmhtdncmjhmjgphmrrhcdrqgmcrzwsznqzpngbtsvjgglrddhjflbrhvqwmmhmqzhphwnvqwzczdvqjsnlhfqbcgddtwgnlcgbfqmzfpqmnbpvfhdhjlnwtrlmggtbfnfvmqrzjvjjvffctsrwgfcpghhnzqmwtlsfhjrvqpwqhngrhpswslsvtgnbvbmwsfwmpntfsfpshrjzvghhpvnlbmnrhltfpmqdwzfhztvhlmbnmhnbvdzbbtczvwbvwtvjghhjjrtgbrqrhmbgvssstdwztdmdsqtctghjhsnpslqttdlvndmjfnmdzwrblfjqcwptfttvlcgsvwcbmfzbdlmrtchgqlfspwznbzfjthjtfwshqgfsfdsmzsmpptzschlzjshvfwtmpszvrvlggbrgpcnqwndhjjprztdfddblhfljbvttfvhchhdfsftrhccrbncmhwpcpwfqthngcqptmvsmpcswdrdlcbqvvhwmcqqwbzlblrgfcrrndwdvlvnpjvwchzjzmgrqhzzmgqqdsdflpclpdtlhvhcthzjfbvjvzsnbvwfsnglvbnwnbgrqwpbgclhjhztttbjwvmlmmgmzncbwswncqhmcfjfnwnpbrmchhpgwngrfwgdfdqmblwlghdjvdhjftdblrtcvvgbvpmbjhfwgpmghqbqrcpgfvhtvqtlbjdblggcpjzlrhpbsqwntfhbhwwszpdlsgbpfqhvrjrhsldcgvqhqmwdfcrcmhrvvwvbrfsrrcvwzhqqvgltlnhwhdrhrdqsvmdzjwgmqdsccwhcgwltfhdfqpsltjccwsttmrc"

### creating empty data frame to track each marker

``` r
df = data.frame(marker = rep(NA, times = nchar(input) - 3), marker_num = NA, n_processed = NA)

head(df)
```

    ##   marker marker_num n_processed
    ## 1     NA         NA          NA
    ## 2     NA         NA          NA
    ## 3     NA         NA          NA
    ## 4     NA         NA          NA
    ## 5     NA         NA          NA
    ## 6     NA         NA          NA

### creating function to check if four characters are all different

``` r
check_diff = function(x) {
  
  return(substr(x, 1, 1) != substr(x, 2, 2) & substr(x, 1, 1) != substr(x, 3, 3) &
           substr(x, 1, 1) != substr(x, 4, 4) & substr(x, 2, 2) != substr(x, 3, 3) &
           substr(x, 2, 2) != substr(x, 4, 4) & substr(x, 3, 3) != substr(x, 4, 4))
  
}

check_diff("abcc")
```

    ## [1] FALSE

``` r
check_diff("abcd")
```

    ## [1] TRUE

### filling data frame with each four-character marker

``` r
for (i in 1:nrow(df)) {
  
  df$marker[i] = substr(input, i, i + 3)
  df$marker_num[i] = i
  df$n_processed[i] = i + 3
  
}

head(df)
```

    ##   marker marker_num n_processed
    ## 1   mgww          1           4
    ## 2   gwwj          2           5
    ## 3   wwjd          3           6
    ## 4   wjdd          4           7
    ## 5   jddq          5           8
    ## 6   ddqz          6           9

### applying function to data

``` r
df = df |>
  mutate(all_different = check_diff(marker))

head(df)
```

    ##   marker marker_num n_processed all_different
    ## 1   mgww          1           4         FALSE
    ## 2   gwwj          2           5         FALSE
    ## 3   wwjd          3           6         FALSE
    ## 4   wjdd          4           7         FALSE
    ## 5   jddq          5           8         FALSE
    ## 6   ddqz          6           9         FALSE

### part 1 solution

``` r
df |>
  filter(all_different) |>
  pull(n_processed) |>
  head(1)
```

    ## [1] 1544

### recreating data frame with all possible markers

``` r
df = data.frame(marker = rep(NA, times = nchar(input) - 13), marker_num = NA, n_processed = NA)

head(df)
```

    ##   marker marker_num n_processed
    ## 1     NA         NA          NA
    ## 2     NA         NA          NA
    ## 3     NA         NA          NA
    ## 4     NA         NA          NA
    ## 5     NA         NA          NA
    ## 6     NA         NA          NA

### creating function to check if all fourteen characters are different

``` r
check_diff2 = function(x) {
  
  count = data.frame(letters = str_split(x, pattern = "")[[1]]) |>
    count(letters) |>
    nrow()

  return(count == nchar(x))
  
}

check_diff2("abcdefghijklmm")
```

    ## [1] FALSE

``` r
check_diff2("abcdefghijklmn")
```

    ## [1] TRUE

### filling data frame with each fourteen-character marker

``` r
for (i in 1:nrow(df)) {
  
  df$marker[i] = substr(input, i, i + 13)
  df$marker_num[i] = i
  df$n_processed[i] = i + 13
  
}

head(df)
```

    ##           marker marker_num n_processed
    ## 1 mgwwjddqzqdqss          1          14
    ## 2 gwwjddqzqdqsst          2          15
    ## 3 wwjddqzqdqsstc          3          16
    ## 4 wjddqzqdqsstct          4          17
    ## 5 jddqzqdqsstctj          5          18
    ## 6 ddqzqdqsstctjj          6          19

### applying function to data

``` r
df = df |>
  rowwise() |>
  mutate(all_different = check_diff2(marker))

head(df)
```

    ## # A tibble: 6 x 4
    ## # Rowwise: 
    ##   marker         marker_num n_processed all_different
    ##   <chr>               <int>       <dbl> <lgl>        
    ## 1 mgwwjddqzqdqss          1          14 FALSE        
    ## 2 gwwjddqzqdqsst          2          15 FALSE        
    ## 3 wwjddqzqdqsstc          3          16 FALSE        
    ## 4 wjddqzqdqsstct          4          17 FALSE        
    ## 5 jddqzqdqsstctj          5          18 FALSE        
    ## 6 ddqzqdqsstctjj          6          19 FALSE

### part 2 solution

``` r
df |>
  filter(all_different) |>
  pull(n_processed) |>
  head(1)
```

    ## [1] 2145
