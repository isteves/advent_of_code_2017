library(tidyverse)
library(here)

input <- read.csv(here("07_testdata.csv"), header = FALSE)

input_sep <- input %>% 
    separate(V1, into = c("base", "weight"), sep = "[(]") %>% 
    separate(weight, into = c("weight", "upper"), sep = "[)]")

#first Q: what is the bottom program?
#1. compile all upper names into a vector
upper <- paste(input_sep$upper, collapse = " ")
upper <- str_extract_all(upper, boundary("word"))[[1]]
base <- paste(input_sep$base, collapse = " ")
base <- str_extract_all(base, boundary("word"))[[1]]

#2. add to all base names --> any program with no duplicate is the base1
all_names <- c(base, upper)
base1 <- table(all_names)[table(all_names) == 1]
print(base1)

# now try to use puzzle input ---------------

input <- read.csv(here("07_data.csv"), header = FALSE)

input_sep <- input %>% 
    separate(V1, into = c("base", "weight"), sep = "[(]") %>% 
    separate(weight, into = c("weight", "upper"), sep = "[)]")

#first Q: what is the bottom program?
#1. compile all upper names into a vector
upper <- paste(input_sep$upper, collapse = " ")
upper <- str_extract_all(upper, boundary("word"))[[1]]
base <- paste(input_sep$base, collapse = " ")
base <- str_extract_all(base, boundary("word"))[[1]]

#2. add to all base names --> any program with no duplicate is the base1
all_names <- c(base, upper)
base1 <- table(all_names)[table(all_names) == 1]
print(base1)

#PART 2 ---------------------
input <- read.csv(here("07_testdata.csv"), header = FALSE)

input_sep <- input %>% 
    separate(V1, into = c("base", "weight"), sep = " [(]") %>% #added space to sep
    separate(weight, into = c("weight", "upper"), sep = "[)]")  
    # mutate(upper = str_extract_all(upper, boundary("word"))) #make this neater

trios <- input_sep %>% filter(upper != "") %>% select(upper)
trios$status <- NA
for(i in 1:nrow(trios)){
    group <- trios[i,]
    group <- str_extract_all(group, boundary("word"))[[1]]
    weights <- input_sep$weight[which(input_sep$base %in% group)]
    trios$weights <- sum(as.numeric(weights))
    if(sum(duplicated(weights)) == length(weights) - 1){
        trios$status <- TRUE
    } else {
        trios$status <- FALSE
    }
}

print(trios[trios$status == FALSE,])

#hmm I'm getting stuck...
#let's try figuring out what the tips of the trees are, and go from there...
#starting from the tips, i'll figure out the weights of each successive level

#for every base without upper --> replace uppers with weights
#then remove these from the equation
#let's try it once...
for(i in row(input_sep)){
    if(length(input_sep$upper[[i]]) < 1) #if the upper section is empty...
        
}

tips <- input_sep %>% 
    filter(upper == "") %>% 
    select(-upper) %>% 
    mutate(weight = as.numeric(weight))
bases <- input_sep %>% filter(upper != "") %>% 
    mutate(weight = as.numeric(weight))
while(nrow(bases) > 0){
    for(i in 1:nrow(bases)){
        uppers <- str_extract_all(bases$upper[i], boundary("word"))[[1]]
        if(sum(uppers %in% tips$base) == length(uppers)){ #all are tips
            sum_weight <- sum(tips$weight[tips$base %in% uppers])
            #add to tips
            tips <- tips %>% add_row(base = bases$base[i],
                                     weight = sum_weight + bases$weight[i])
            
            #delete from bases
            bases <- bases[-i,]
        }
    }
}

#now I can run the trio matching from before...
trios <- input_sep %>% filter(upper != "") %>% select(upper)
trios$status <- NA
for(i in 1:nrow(trios)){
    group <- trios[i,]
    group <- str_extract_all(group, boundary("word"))[[1]]
    weights <- tips$weight[which(tips$base %in% group)]
    trios$weight[i] <- sum(weights)
    if(sum(duplicated(weights)) == length(weights) - 1){
        trios$status[i] <- TRUE
    } else {
        trios$status[i] <- FALSE
        print(group)
        print(weights)
    }
}

#seems to work...!  now let's try with the puzzle input...

# ----------

input <- read.csv(here("07_data.csv"), header = FALSE)

input_sep <- input %>% 
    separate(V1, into = c("base", "weight"), sep = " [(]") %>% #added space to sep
    separate(weight, into = c("weight", "upper"), sep = "[)]")  
# mutate(upper = str_extract_all(upper, boundary("word"))) #make this neater

tips <- input_sep %>% 
    filter(upper == "") %>% 
    select(-upper) %>% 
    mutate(weight = as.numeric(weight))
bases <- input_sep %>% filter(upper != "") %>% 
    mutate(weight = as.numeric(weight))
while(nrow(bases) > 0){
    for(i in 1:nrow(bases)){
        uppers <- str_extract_all(bases$upper[i], boundary("word"))[[1]]
        if(sum(uppers %in% tips$base) == length(uppers)){ #all are tips
            sum_weight <- sum(tips$weight[tips$base %in% uppers])
            #add to tips
            tips <- tips %>% add_row(base = bases$base[i],
                                     weight = sum_weight + bases$weight[i])
            
            #delete from bases
            bases <- bases[-i,]
        }
    }
}

#now I can run the trio matching from before...
trios <- input_sep %>% filter(upper != "") %>% select(upper)
trios$status <- NA
for(i in 1:nrow(trios)){
    group <- trios[i,]
    group <- str_extract_all(group, boundary("word"))[[1]]
    weights <- tips$weight[which(tips$base %in% group)]
    trios$weight[i] <- sum(weights)
    if(sum(duplicated(weights)) == length(weights) - 1){
        trios$status[i] <- TRUE
    } else {
        trios$status[i] <- FALSE
        print(tips[which(tips$base %in% group),])
    }
}

#wrong program appears to be apjxafk; it needs to be 8 units lighter
input_sep %>% filter(base == "apjxafk")
1513-8
