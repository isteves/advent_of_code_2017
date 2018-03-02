Advent of Code: Day 3
================
Irene Steves
February 16, 2018

Compared to [Day 3](https://adventofcode.com/2017/day/3), Days 1 and 2 were a breeze! It definitely put that growing coding ego in check.

Part I
------

    You come across an experimental new kind of memory stored on an infinite two-dimensional
    grid.

    Each square on the grid is allocated in a spiral pattern starting at a location marked 
    1 and then counting up while spiraling outward. For example, the first few squares are
    allocated like this:

    17  16  15  14  13
    18   5   4   3  12
    19   6   1   2  11
    20   7   8   9  10
    21  22  23---> ...
    While this is very space-efficient (no squares are skipped), requested data must be
    carried back to square 1 (the location of the only access port for this memory system)
    by programs that can only move up, down, left, or right. They always take the shortest
    path: the Manhattan Distance between the location of the data and square 1.

    For example:

    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.
    How many steps are required to carry the data from the square identified in your puzzle
    input all the way to the access port?

After chasing down a few dead ends, I came up with this approach:

Since we care primarily about position, we can re-write our spiral from above as steps from the center (zero):

    6   5   4   3   4   5   6
    5   4   3   2   3   4   5
    4   3   2   1   2   3   4
    3   2   1   0   1   2   3
    4   3   2   1   2   3   4
    5   4   3   2   3   4   5
    6   5   4   3   4  -->

We can think of these numbers as a series of successive square-shaped frames around 0. If we write it out for each frame, then we get:

    Frame 1: 0
    Frame 2: 1 2 1 2 1 2 1 2
    Frame 3: 3 2 3 4 3 2 3 4 3 2 3 4 3 2 3 4
    Frame 4: 5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6
    Frame n: .....

From this, we see that:

-   Each frame (after frame 1) ends with an even number, specifically, 2(*n* − 1)
-   Each frame (after 1) starts with an odd number, specifically, one less than the even number: 2(*n* − 1)−1 = 2*n* − 3
-   The minimum number of steps in each frame is equal to *n* − 1
-   Except for frame 1, the number of digits in each frame is divisible by 4 (since there are four sides in a frame)

<!-- -->

    Frame 1: 0
    Frame 2: (1 2) (1 2) (1 2) (1 2)
    Frame 3: (3 2 3 4) (3 2 3 4) (3 2 3 4) (3 2 3 4)
    Frame 4: (5 4 3 4 5 6) (5 4 3 4 5 6) (5 4 3 4 5 6) (5 4 3 4 5 6)

Now, we can start pulling some of these patterns together:

``` r
library(tidyverse)
steps <- tibble(frame = 1:10) %>% 
    mutate(max_steps = 2*(frame - 1),
           min_steps = frame - 1,
           start_steps = max_steps - 1,
           end_steps = max_steps) 
print(steps)
```

    ## # A tibble: 10 x 5
    ##    frame max_steps min_steps start_steps end_steps
    ##    <int>     <dbl>     <dbl>       <dbl>     <dbl>
    ##  1     1         0         0          -1         0
    ##  2     2         2         1           1         2
    ##  3     3         4         2           3         4
    ##  4     4         6         3           5         6
    ##  5     5         8         4           7         8
    ##  6     6        10         5           9        10
    ##  7     7        12         6          11        12
    ##  8     8        14         7          13        14
    ##  9     9        16         8          15        16
    ## 10    10        18         9          17        18

Except for the first frames, we know that for each side, the number of steps starts at some number (`start_steps`), decreases by one until it gets to the minimum number of steps (`min_steps`), and then increases by one until it gets to the end, or maximum, number of steps (`end_steps` or `max_steps`). Repeat this four times and we have the whole sequence of steps for each frame.

``` r
positions <- steps %>% 
    rowwise() %>% 
    mutate(seq = paste(c(start_steps:min_steps, (min_steps + 1):end_steps), 
                       collapse = " "),
           seq4 = paste(rep(seq, 4), collapse = " ")) %>% 
    mutate(seq4 = replace(seq4, frame == 1, 0)) %>%  #replace square 1's sequence with 0
    select(seq4)
print(positions)
```

    ## Source: local data frame [10 x 1]
    ## Groups: <by row>
    ## 
    ## # A tibble: 10 x 1
    ##                                                                           seq4
    ##                                                                          <chr>
    ##  1                                                                           0
    ##  2                                                             1 2 1 2 1 2 1 2
    ##  3                                             3 2 3 4 3 2 3 4 3 2 3 4 3 2 3 4
    ##  4                             5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6
    ##  5             7 6 5 4 5 6 7 8 7 6 5 4 5 6 7 8 7 6 5 4 5 6 7 8 7 6 5 4 5 6 7 8
    ##  6 9 8 7 6 5 6 7 8 9 10 9 8 7 6 5 6 7 8 9 10 9 8 7 6 5 6 7 8 9 10 9 8 7 6 5 6 
    ##  7 11 10 9 8 7 6 7 8 9 10 11 12 11 10 9 8 7 6 7 8 9 10 11 12 11 10 9 8 7 6 7 8
    ##  8 13 12 11 10 9 8 7 8 9 10 11 12 13 14 13 12 11 10 9 8 7 8 9 10 11 12 13 14 1
    ##  9 15 14 13 12 11 10 9 8 9 10 11 12 13 14 15 16 15 14 13 12 11 10 9 8 9 10 11 
    ## 10 17 16 15 14 13 12 11 10 9 10 11 12 13 14 15 16 17 18 17 16 15 14 13 12 11 1

If you now paste it all together as a long string (with a tiny bit of base R), and then separate out again into a vector of integers, we can answer the puzzle question!

``` r
str_c(positions$seq4, collapse = " ") %>% 
    str_split(" ", simplify = TRUE) %>% 
    as.numeric()
```

    ##   [1]  0  1  2  1  2  1  2  1  2  3  2  3  4  3  2  3  4  3  2  3  4  3  2
    ##  [24]  3  4  5  4  3  4  5  6  5  4  3  4  5  6  5  4  3  4  5  6  5  4  3
    ##  [47]  4  5  6  7  6  5  4  5  6  7  8  7  6  5  4  5  6  7  8  7  6  5  4
    ##  [70]  5  6  7  8  7  6  5  4  5  6  7  8  9  8  7  6  5  6  7  8  9 10  9
    ##  [93]  8  7  6  5  6  7  8  9 10  9  8  7  6  5  6  7  8  9 10  9  8  7  6
    ## [116]  5  6  7  8  9 10 11 10  9  8  7  6  7  8  9 10 11 12 11 10  9  8  7
    ## [139]  6  7  8  9 10 11 12 11 10  9  8  7  6  7  8  9 10 11 12 11 10  9  8
    ## [162]  7  6  7  8  9 10 11 12 13 12 11 10  9  8  7  8  9 10 11 12 13 14 13
    ## [185] 12 11 10  9  8  7  8  9 10 11 12 13 14 13 12 11 10  9  8  7  8  9 10
    ## [208] 11 12 13 14 13 12 11 10  9  8  7  8  9 10 11 12 13 14 15 14 13 12 11
    ## [231] 10  9  8  9 10 11 12 13 14 15 16 15 14 13 12 11 10  9  8  9 10 11 12
    ## [254] 13 14 15 16 15 14 13 12 11 10  9  8  9 10 11 12 13 14 15 16 15 14 13
    ## [277] 12 11 10  9  8  9 10 11 12 13 14 15 16 17 16 15 14 13 12 11 10  9 10
    ## [300] 11 12 13 14 15 16 17 18 17 16 15 14 13 12 11 10  9 10 11 12 13 14 15
    ## [323] 16 17 18 17 16 15 14 13 12 11 10  9 10 11 12 13 14 15 16 17 18 17 16
    ## [346] 15 14 13 12 11 10  9 10 11 12 13 14 15 16 17 18

We can't quite get to the puzzle answer with just the first 10 frames, so we'll have to expand our scope a little. Let's pull everything together in one giant pipe:

``` r
positions500 <- tibble(frame = 1:500) %>% 
    mutate(max_steps = 2*(frame - 1),
           min_steps = frame - 1,
           start_steps = max_steps - 1,
           end_steps = max_steps) %>% 
    
    rowwise() %>% 
    mutate(seq = paste(c(start_steps:min_steps, (min_steps + 1):end_steps), 
                       collapse = " "),
           seq4 = paste(rep(seq, 4), collapse = " ")) %>% 
    mutate(seq4 = replace(seq4, frame == 1, 0)) %>%  #replace frame 1's sequence with 0
    select(seq4) %>% 
    
    str_c(collapse = " ") %>% 
    #directly using str_c on the tibble, rather than tibble$seq4 
    #results in the inclusion of some special characters
    #so, rather than str_split at all the spaces,
    #we can extract all digits instead:
    str_extract_all("\\d+", simplify = TRUE) %>% 
    as.numeric()
```

Let's run some tests to make sure it works!

``` r
positions500[1] == 0; positions500[12] == 3; positions500[23] == 2; positions500[1024] == 31
```

    ## [1] TRUE

    ## [1] TRUE

    ## [1] TRUE

    ## [1] TRUE
