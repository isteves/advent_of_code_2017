Advent of Code: Day 1
================
Irene Steves
February 16, 2018

The 2017 Advent of Code may already be two months behind us, but I figured it's still not too late to write up some of my solutions for the challenge. It was my first time participating, and I managed to finish about 10 of them before the holiday festivities (and thesis writing) caught up with me.

As almost purely an R User, my answers are all R based. It may not be the neatest for certain puzzles, but you can always make it work somehow!

Part I
------

Let's get started with the challenge from [Day 1](https://adventofcode.com/2017/day/1). Here's the key part:

    The captcha requires you to review a sequence of digits (your puzzle input) and
    find the sum of all digits that match the next digit in the list. The list is
    circular, so the digit after the last digit is the first digit in the list.

    For example:
        
    - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the 
    second digit and the third digit (2) matches the fourth digit.
    - 1111 produces 4 because each digit (all 1) matches the next.
    - 1234 produces 0 because no digit matches the next.
    - 91212129 produces 9 because the only digit that matches the next one is 
    the last digit, 9.

In short, for any series of numbers, the "captcha" is the sum of all digits that match the next digit in the series. If the last digit matches the first digit, then it also gets added into the sum.

I ended up taking the following approach (which you can review in the code chunk below):

1.  Split the numeric string into a vector of digits using `str_split`
2.  For each digit, if the digit was equal to the next digit in the series, then I would add that value to my `sum`.
3.  Since my first and last digit was 8, I added 8 to the final sum.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.4.3

    ## -- Attaching packages ---------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.4
    ## v tibble  1.3.4     v dplyr   0.7.4
    ## v tidyr   0.7.2     v stringr 1.2.0
    ## v readr   1.1.1     v forcats 0.2.0

    ## Warning: package 'ggplot2' was built under R version 3.4.3

    ## Warning: package 'tibble' was built under R version 3.4.3

    ## Warning: package 'tidyr' was built under R version 3.4.3

    ## Warning: package 'readr' was built under R version 3.4.3

    ## Warning: package 'purrr' was built under R version 3.4.3

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## Warning: package 'stringr' was built under R version 3.4.3

    ## Warning: package 'forcats' was built under R version 3.4.3

    ## -- Conflicts ------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
x <- "8231753674683997878179259195565332579493378483264978"
x_vector <- str_split(x, "")[[1]]

sum <- 0
for(i in 1:(length(x_vector) - 1)){
    if(x_vector[i] == x_vector[i + 1]){
        sum <- sum + as.numeric(x_vector[i])
    }
}

answer <- sum + 8
```

Part 2
------

Part 2 is almost the same, but requires a few modifications:

    Now, instead of considering the next digit, it wants you to consider the digit
    halfway around the circular list. That is, if your list contains 10 items, only
    include a digit in your sum if the digit 10/2 = 5 steps forward matches it.
    Fortunately, your list has an even number of elements.

    For example:

    - 1212 produces 6: the list contains 4 items, and all four digits match the
    digit 2 items ahead.
    - 1221 produces 0, because every comparison is between a 1 and a 2.
    - 123425 produces 4, because both 2s match each other, but no other digit has 
    a match.
    - 123123 produces 12.
    - 12131415 produces 4.

We start with the same input (`x`) as before, but we need a few modifications:

1.  To make circling around the list easy, we can double `x_vector`
2.  Now rather than compare `i` and `i + 1`, we're interested in `i` and `i + l/2`. (English: any digit and the digit halfway around the circular list).
3.  Save as a sum like before! No need to add 8 since we're checking on all our digits (from 1 to the total length of `x_vector`) in our for-loop.

<!-- -->

    ## [1] 52

Not so bad for Day 1, eh?
