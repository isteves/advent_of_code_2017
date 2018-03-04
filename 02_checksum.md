Advent of Code: Day 2
================
Irene Steves
February 16, 2018

Looking back on these challenges, I realize that many (perhaps all?) of these challenges were based on real computer-science concepts and theory! The `checksum` is one of them...

Part I
------

[Day 2's](https://adventofcode.com/2017/day/2) challenge describes a relatively simple checksum process:

    The spreadsheet consists of rows of apparently-random numbers. To make sure the
    recovery process is on the right track, they need you to calculate the
    spreadsheet's checksum. For each row, determine the difference between the
    largest value and the smallest value; the checksum is the sum of all of these
    differences.

    For example, given the following spreadsheet:

    5 1 9 5
    7 5 3
    2 4 6 8

    The first row's largest and smallest values are 9 and 1, and their difference 
    is 8.
    The second row's largest and smallest values are 7 and 3, and their difference
    is 4.
    The third row's difference is 6.
    In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

When solving a puzzle (or building a function), it's often easiest to consider the simplest case first. But sometimes, you just feel like diving directly into the mud, which is how I felt the first time around. I saved the puzzle input to a txt file in Notepad, opened it in Excel to save it to a csv, and then read it into R using `read.csv`. That's a heck of a lot of manual steps! I realize now that R is smart enough to understand how txt files are delimitted (see help for `read.table`), and that it's way easier to test code on a simple example that you know the answer to, rather than debugging by testing it on a giant monster.

So, to do it the "proper" way, we start with code that works on a simple example:

``` r
data <- rbind(c(5, 1, 9, 5),
              c(7, 5, 3, NA),
              c(2, 4, 6, 8))

diff_vec <- NULL
for(i in 1:nrow(data)){
    diff <- max(data[i,]) - min(data[i,])
    diff_vec <- c(diff_vec, diff)
}

checksum <- sum(diff_vec)
```

When processing speed isn't a problem, I default to for-loops because they jive well with my thought process (some later Advent of Code challenges will have some examples in which you want to avoid use of for-loops as much as possible). In the code chunk above, I initialized `diff_vec` to save the difference of each row. I then went row by row (from 1 to the number of rows in `data`) to calculate the difference. In the end, I summed all the differences that I'd saved in `diff_vec`.

Once you get the simple data working, all you have to do is replace `data` with the more complicated puzzle input.

*Note:* efficient coders would prefer to--at minimum--initialize `diff_vec` as `vector(length = nrow(data))` and then fill in each slot `diff_vec[i]` as I calculate it. An analogy: you need an office mail organizer with enough spots for each professor in the department to have their own slot. Rather than building a new slot each time a professor gets mail for the first time, you can buy a giant mail organizer and assign each professor a slot as you go.

<img src="http://www.safcoproducts.com/wcsstore/ExtendedSitesCatalogAssetStore/htdocs/productMgr/img_hr/7766GR_hr.jpg" style="width:50.0%" />

Part II
-------

Oh no! It says that `the program seems a little worried`. :worried:

    It sounds like the goal is to find the only two numbers in each row where one evenly
    divides the other - that is, where the result of the division operation is a whole
    number. They would like you to find those numbers on each line, divide them, and add up
    each line's result.

    For example, given the following spreadsheet:

    5 9 2 8
    9 4 7 3
    3 8 6 5
    In the first row, the only two numbers that evenly divide are 8 and 2; the result of
    this division is 4.
    In the second row, the two numbers are 9 and 3; the result is 3.
    In the third row, the result is 2.
    In this example, the sum of the results would be 4 + 3 + 2 = 9.

    What is the sum of each row's result in your puzzle input?

Let's break it down a little:

-   like before, we want a value for each row
-   like before, we only care about two numbers in each row
-   UNlike before, we're doing some division...

*Disclaimer:* Coding is fun because there are often lots of ways of doing the same thing! For these challenges, most of the code is based on whatever pops into my head first.

So, how might we check for evenly dividing numbers? The `%%` operator fits the bill! It calculates the remainder of one number divided by another. Here are some examples:

``` r
5 %% 2
```

    ## [1] 1

``` r
4 %% 2
```

    ## [1] 0

``` r
11 %% 3
```

    ## [1] 2

Now to apply it to our example, let's take a simple vector, `v`. If we divide `v` by the first number in the vector (`v[1]` or "5"), the remainder is only zero when it is divided by itself. When `v` is divided by "2" (`v[3]`), on the other hand, the remainder for *both* 2 and 8 is zero.

``` r
v <- c(5, 9, 2, 8)
v %% v[1]
```

    ## [1] 0 4 2 3

``` r
v %% v[3]
```

    ## [1] 1 1 0 0

Thus, the element of the vector that produces *two* zeroes when `v %% v[i]` must be the smaller of the two evenly divisible numbers. Let's write up a function that can identify these numbers and then divide them.

``` r
even_div <- function(numbers) {
    for(i in 1:length(numbers)){ 
        n_remainder <- numbers %% numbers[i]
        
        #if the # of zeroes is two...
        if(sum(n_remainder == 0) == 2){ 
            
            #define position of the big and small number to divide evenly
            positions <- which(n_remainder == 0)
            pos_small <- i
            pos_big <- positions[which(positions != i)]
        }
    }
    numbers[pos_big]/numbers[pos_small]
}
```

Now that we have the function, we can apply it to all rows, and then sum it up! Challenge completed.

``` r
data <- rbind(c(5, 2, 9, 8),
              c(9, 4, 7, 3),
              c(3, 8, 6, 5))

quotients <- apply(data, 1, even_div)
sum(quotients)
```

    ## [1] 9
