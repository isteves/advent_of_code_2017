Advent of Code: Day 3
================
Irene Steves
March 3, 2018

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
-   The maximum number of steps is equal to the last number in the frame, or 2(*n* − 1)
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

If we now paste it all together as a long string (with a tiny bit of base R), and then separate out again into a vector of integers, we can answer the puzzle question!

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
    
    str_c(collapse = " ") %>% #see NOTE
    str_extract_all("\\d+", simplify = TRUE) %>% 
    as.numeric()
```

*Note:* Directly using `str_c` on the tibble, rather than subsetting the column using something like `tibble$seq4` results in the inclusion of some special characters. So, rather than `str_split` at all spaces (" "), we can extract all digits instead (using this handy [basic regular expressions in R cheatsheet](https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)). *Note 2:* After writing this, I discovered the `separate_rows` function which does the same thing much more elegantly (see part II for use).

Let's run some tests to make sure it works!

``` r
positions500[12] == 3; positions500[23] == 2; positions500[1024] == 31
```

    ## [1] TRUE

    ## [1] TRUE

    ## [1] TRUE

:satisfied: :star2: :fireworks:

Part II
-------

For this section, they make things a bit more complicated...it's no longer only about position.

    As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1. Then, in the same allocation order as shown above, they store the sum of the values in all adjacent squares, including diagonals.

    So, the first few squares' values are chosen as follows:

    Square 1 starts with the value 1.
    Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
    Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
    Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
    Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
    Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

    147  142  133  122   59
    304    5    4    2   57
    330   10    1    1   54
    351   11   23   25   26
    362  747  806--->   ...

    What is the first value written that is larger than your puzzle input?

For this part of the puzzle, it seemed easiest to me to simply go through and build this spiral up from the ground. To start, we need to understand how the spiral moves in terms of coordinates. If we take the center position to be (0, 0), then the next position is (1, 0), then (1, 1), and so on:

    17  16  15  14  13
    18   5   4   3  12
    19   6   1   2  11
    20   7   8   9  10
    21  22  23---> ...

    1 = (0, 0)
    2 = (1, 0)
    3 = (1, 1)
    4 = (0, 1)
    5 = (-1, 1)
    6 = (-1, 0)
    7 = (-1, -1)
    8 = (0, -1)
    9 = (1, -1)
    10 = (2, -1)
    11 = (2, 0)
    12 = (2, 1)
    13 = (2, 2)
    14 = (1, 2)
    15 = (0, 2)

Let's write it out as a data frame:

``` r
df <- data.frame(position = 1:15,
                 x = c(0,1,1,0,-1,-1,-1,0,1,2,2,2,2,1,0),
                 y = c(0,0,1,1,1,0,-1,-1,-1,-1,0,1,2,2,2))
df
```

    ##    position  x  y
    ## 1         1  0  0
    ## 2         2  1  0
    ## 3         3  1  1
    ## 4         4  0  1
    ## 5         5 -1  1
    ## 6         6 -1  0
    ## 7         7 -1 -1
    ## 8         8  0 -1
    ## 9         9  1 -1
    ## 10       10  2 -1
    ## 11       11  2  0
    ## 12       12  2  1
    ## 13       13  2  2
    ## 14       14  1  2
    ## 15       15  0  2

Now let's look at the change in x and y:

``` r
df %>% 
    mutate(start_x = c(NA, x[-15]),
           start_y = c(NA, y[-15]),
           diff_x = x - start_x,
           diff_y = y - start_y) %>% 
    select(position, diff_x, diff_y)
```

    ##    position diff_x diff_y
    ## 1         1     NA     NA
    ## 2         2      1      0
    ## 3         3      0      1
    ## 4         4     -1      0
    ## 5         5     -1      0
    ## 6         6      0     -1
    ## 7         7      0     -1
    ## 8         8      1      0
    ## 9         9      1      0
    ## 10       10      1      0
    ## 11       11      0      1
    ## 12       12      0      1
    ## 13       13      0      1
    ## 14       14     -1      0
    ## 15       15     -1      0

From this, we can see clearly that:

1.  Every change in position is at most 1 step
2.  When x changes, y does not change (and vice versa)
3.  The number of steps flips between series of +1's and -1's (with zeros in between)
4.  The number of repeats expands by one each time: one 1, one 0, two -1's, two 0's, three 1's, three 0's....

I decided to take advantage of these patterns to build up a spiral, step-by-step. I went the for-loop route the first time around:

``` r
total_x <- NULL
for(i in 1:100){
    vector <- c(rep((-1)^(i+1), times = i), 
                rep(0, times = i))
    # i + 1 helps us alternate between +1 and -1
    # we increase the number of times it repeats by one each time we loop
    total_x <- c(total_x, vector)
}

total_y <- NULL
for(i in 1:100){
    vector <- c(rep(0, i), rep((-1)^(i+1), i)) #start with 0 for y
    total_y <- c(total_y, vector)
}

xy <- data.frame(x = c(0,total_x),
                 y = c(0,total_y))
head(xy)
```

    ##    x  y
    ## 1  0  0
    ## 2  1  0
    ## 3  0  1
    ## 4 -1  0
    ## 5 -1  0
    ## 6  0 -1

Upon returning to this, I tidyversed it:

``` r
xy <- tibble(i = 1:100) %>% 
    rowwise() %>%
    mutate(x = str_c(c(rep((-1)^(i+1), i), rep(0, i)), 
                     collapse = " "),
           y = str_c(c(rep(0, i), rep((-1)^(i+1), i)), 
                     collapse = " ")) %>% 
    separate_rows(x:y, sep = " ", convert = TRUE)
head(xy)
```

    ## # A tibble: 6 x 3
    ##       i     x     y
    ##   <int> <int> <int>
    ## 1     1     1     0
    ## 2     1     0     1
    ## 3     2    -1     0
    ## 4     2    -1     0
    ## 5     2     0    -1
    ## 6     2     0    -1

Let's test this out on a small matrix to make sure the spiral builds correctly.

``` r
grid <- matrix(nrow = 5, ncol = 5)

i = 3; j = 3 #starting point (origin)
for(n in 1:10){
    i <- i - xy$y[n]
    j <- j + xy$x[n]
    grid[i, j] <- n
    print(grid) #just to see that we're doing this right
}
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA   NA   NA   NA   NA
    ## [3,]   NA   NA   NA    1   NA
    ## [4,]   NA   NA   NA   NA   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA   NA   NA    2   NA
    ## [3,]   NA   NA   NA    1   NA
    ## [4,]   NA   NA   NA   NA   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA   NA    3    2   NA
    ## [3,]   NA   NA   NA    1   NA
    ## [4,]   NA   NA   NA   NA   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA    4    3    2   NA
    ## [3,]   NA   NA   NA    1   NA
    ## [4,]   NA   NA   NA   NA   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA    4    3    2   NA
    ## [3,]   NA    5   NA    1   NA
    ## [4,]   NA   NA   NA   NA   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA    4    3    2   NA
    ## [3,]   NA    5   NA    1   NA
    ## [4,]   NA    6   NA   NA   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA    4    3    2   NA
    ## [3,]   NA    5   NA    1   NA
    ## [4,]   NA    6    7   NA   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA    4    3    2   NA
    ## [3,]   NA    5   NA    1   NA
    ## [4,]   NA    6    7    8   NA
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA    4    3    2   NA
    ## [3,]   NA    5   NA    1   NA
    ## [4,]   NA    6    7    8    9
    ## [5,]   NA   NA   NA   NA   NA
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]   NA   NA   NA   NA   NA
    ## [2,]   NA    4    3    2   NA
    ## [3,]   NA    5   NA    1   10
    ## [4,]   NA    6    7    8    9
    ## [5,]   NA   NA   NA   NA   NA

It seems to work! Now instead of inputting the step number to the grid, let's do some addition:

``` r
grid <- matrix(nrow = 7, ncol = 7) #start with a slightly larger grid
i = 4; j = 4 #origin coordinates
grid[i, j] <- 1 #define origin as 1

for(n in 2:20){ #starts at 2 since we already defined the origin
    i <- i - xy$y[n]
    j <- j + xy$x[n]
    
    #get sum of neighbors
    neighbors <- grid[(i - 1):(i + 1), (j - 1):(j + 1)]
    sum_neighbors <- sum(neighbors, na.rm = TRUE)
    
    grid[i, j] <- sum_neighbors
}
```

Again, it seems to work! Now to answer the puzzle, I'll expand the grid and also save the output as a vector.

``` r
grid <- matrix(nrow = 1001, ncol = 1001)
i = 501; j = 501
grid[i, j] <- 1

v_sum_neighbors <- vector(length = 1000)
for(n in 2:1000){
    i <- i - xy$y[n]
    j <- j + xy$x[n]
    
    neighbors <- grid[(i - 1):(i + 1), (j - 1):(j + 1)]
    sum_neighbors <- sum(neighbors, na.rm = TRUE)
    
    grid[i, j] <- sum_neighbors
    v_sum_neighbors[n] <- sum_neighbors
}
```

Now we're ready to answer the puzzle question: What is the first value written that is larger than your puzzle input?

``` r
puzzle_input <- 312051
head(v_sum_neighbors[v_sum_neighbors > puzzle_input], 1)
```

    ## [1] 322952

Voila! and that's it! Another challenge down. :trophy:
