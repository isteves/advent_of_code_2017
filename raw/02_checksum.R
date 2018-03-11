library(here)

data <- read.csv(here("02_data.csv"), header = FALSE)

diffv <- NULL
for(i in 1:16){
    diff <- max(data[i,]) - min(data[i,])
    diffv <- c(diffv, diff)
}
checksum <- sum(diffv)

#PART 2 ------------
#check evenly dividing numbers
v <- c(5, 9, 2, 8)
v %% v[1]
#want output with two 0's --> save those numbers!

divide <- function(numbers){
    return(max(numbers)/min(numbers))
}

even_div <- function(numbers) {
    for(i in 1:length(numbers)){
        n_remainder <- numbers %% numbers[i]
        if(sum(n_remainder == 0) == 2){ #if the # of zeroes is two...
            good <- numbers[n_remainder == 0]
        }
    }
    divide(good)
}

#apply to all...& sum up!
sum(apply(data, 1, even_div))
