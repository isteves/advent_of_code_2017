#redistribution of memory banks

input <- c(0, 2, 7, 0)
history <- paste(input, collapse = "")
#write out redistribution...
while(any(duplicated(history)) ==  FALSE){ #no repeated distributions/not yet looping
    maxBank <- max(input)
    pos <- which(input == maxBank)[1] #position of maxBank
    #take first (lower) position to break ties
    input[pos] <- 0 #take away all blocks to redistribute
    for(i in 1:maxBank){
        pos <- pos + 1 
        #deal with looping...
        if(pos > length(input)){
            pos <- pos - length(input)
        }
        
        input[pos] <- input[pos] + 1
    }
    print(input)
    history <- c(history, paste(input, collapse = "")) #add input as string
}

steps <- length(history) - 1
print(steps)


##puzzle input
#processing...
input <- "14	0	15	12	11	11	3	5	1	6	8	4	9	1	8	4"
input <- str_split(input, "\t")[[1]]
input <- as.integer(input)

history <- paste(input, collapse = "")
#write out redistribution...
while(any(duplicated(history)) ==  FALSE){ #no repeated distributions/not yet looping
    maxBank <- max(input)
    pos <- which(input == maxBank)[1] #position of maxBank
    #take first (lower) position to break ties
    input[pos] <- 0 #take away all blocks to redistribute
    for(i in 1:maxBank){
        pos <- pos + 1 
        #deal with looping...
        if(pos > length(input)){
            pos <- pos - length(input)
        }
        
        input[pos] <- input[pos] + 1
    }
    print(input)
    history <- c(history, paste(input, collapse = "")) #add input as string
}

steps <- length(history) - 1
print(steps)

#what's position of the sequence that matches the last sequence?
twins <- which(history == history[length(history)]) 
diff(twins)