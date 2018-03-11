#try with test case...
input <- c(0, 3, 0, 1, -3)

pos1 = 1
step = 0
while(pos1 <= length(input) && pos1 > 0){
    pos2 <- pos1 + input[pos1]
    input[pos1] <- input[pos1] + 1
    pos1 <- pos2
    step <- step + 1
}

print(step)

## now use puzzle input

#try with test case...
library(here)
input <- read.csv(here("05_input.csv"), header = FALSE)
input <- input[[1]]

pos1 = 1
step = 0
while(pos1 <= length(input) && pos1 > 0){
    pos2 <- pos1 + input[pos1]
    input[pos1] <- input[pos1] + 1
    pos1 <- pos2
    step <- step + 1
}

print(step)

#part 2 -------------
#try with test case...
input <- c(0, 3, 0, 1, -3)

pos1 = 1
step = 0
while(pos1 <= length(input) && pos1 > 0){
    pos2 <- pos1 + input[pos1]
    if(input[pos1] < 3){
        input[pos1] <- input[pos1] + 1
    } else{
        input[pos1] <- input[pos1] - 1
    }
    pos1 <- pos2
    print(input)
    step <- step + 1
}

print(step)

## ok now with puzzle input...
input <- read.csv(here("05_input.csv"), header = FALSE)
input <- input[[1]]

pos1 = 1
step = 0
while(pos1 <= length(input) && pos1 > 0){
    pos2 <- pos1 + input[pos1]
    if(input[pos1] < 3){
        input[pos1] <- input[pos1] + 1
    } else{
        input[pos1] <- input[pos1] - 1
    }
    pos1 <- pos2
    step <- step + 1
}

print(step)

