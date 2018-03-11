#312051

#Let's start by trying to understand the pattern...
    # the bottom right corner for each successive square 
    # will always be the square of the next odd number

library(tidyverse)
frame <- tibble(n = 1:1000) %>% #number of each square
    mutate(odd = 2*n - 1, #sequence of odd numbers
           rt_corner = odd^2, #square of odd number = # in bottom right corner
           steps = 2 * n) #number of steps to center

#my number is within the first 1000 squares
#in fact, it's somewhere hidden in here...
frame %>%  
    filter(rt_corner > 310000,
           rt_corner < 312500)

#Let's try looking at it another way...
#for a given square x, what is the pattern?
#x = 1: 0 -> 1 digit
#x = 2: 1 2 1 2 1 2 1 2 -> 2 * 4 = 8 digits
#x = 3: 3 2 3 4 3 2 3 4 3 2 3 4 3 2 3 4 -> 4 * 4 = 16 digits
#x = 4: 5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6 5 4 3 4 5 6 -> 6 * 4 = 24 digits
#x = X: ___ -> (2*(X - 1)) * 4 = 8X - 8

frame2 <- tibble(n = 1:500) %>% #each number in sequence
    mutate(square_no = ceiling((sqrt(n) + 1)/2), #the square it belongs to
           square_digits = 8*square_no - 8) #no. of integers in the square

#now we use absolute values to get the ascending/descending pattern...
all_positions <- c(0)
for(i in 2:500){ #for different square numbers...
    d <- 8*i - 8 #square_digits = 8*square_no - 8
    vector <- abs((-d/8 + 1):(d/8)) + i - 1
    vector4 <- rep(vector, 4) #repeat four times
    all_positions <- c(all_positions, vector4)
}

all_positions[312051]

plot(all_positions)
df <- data.frame(x = 1:500,
                 pos = all_positions[1:500])

ggplot(df) +
    geom_line(aes(x = x, y = pos))

#Part 2 ---------------

#try running it as they describe...


total_x <- NULL
for(i in 1:1000){
    vector <- c(rep((-1)^(i+1), i), rep(0, i))
    total_x <- c(total_x, vector)
}

total_y <- NULL
for(i in 1:1000){
    vector <- c(rep(0, i), rep((-1)^(i+1), i))
    total_y <- c(total_y, vector)
}

xy <- data.frame(x = c(0,total_x),
           y = c(0,total_y))

#test with small matrix
grid <- matrix(nrow = 5, ncol = 5)

#start with...
i = 3; j = 3
for(n in 1:10){
    i <- i - xy$y[n]
    j <- j + xy$x[n]
    grid[i, j] <- n
    print(grid)
}
#^this will print the numbers in order from the center in a spiral pattern

grid <- matrix(nrow = 7, ncol = 7); i = 4; j = 4; grid[4,4] <- 1
#now instead of printing the numbers, we'll do some addition...
for(n in 2:20){
    i <- i - xy$y[n]
    j <- j + xy$x[n]
    
    #get sum of neighbors
    neighbors <- grid[(i - 1):(i + 1), (j - 1):(j + 1)]
    sum_neighbors <- sum(neighbors, na.rm = TRUE)
    
    grid[i, j] <- sum_neighbors
}
#it seems to work! 

#now let's expand it and also save the values as a vector...
grid <- matrix(nrow = 1001, ncol = 1001); i = 501; j = 501; grid[i,j] <- 1
v_sum_neighbors <- NULL
for(n in 2:1000){
    i <- i - xy$y[n]
    j <- j + xy$x[n]
    
    #get sum of neighbors
    neighbors <- grid[(i - 1):(i + 1), (j - 1):(j + 1)]
    sum_neighbors <- sum(neighbors, na.rm = TRUE)
    
    grid[i, j] <- sum_neighbors
    v_sum_neighbors <- c(v_sum_neighbors, sum_neighbors)
}

head(v_sum_neighbors[v_sum_neighbors > 312051])


