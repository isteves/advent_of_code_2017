library(here)

# data <- read.csv(here("04_data.csv"), header = FALSE)
data <- read.csv(here("4aData_idan.csv"), header = FALSE)



tdata <- t(data)
data_valid <- NULL
for(i in 1:ncol(tdata)){
    sub <- tdata[,i] # select column
    sub <- sub[sub != ""] # keep only words; remove empty spaces
    if(any(duplicated(sub)) == TRUE){ #if any words in sub are duplicated
        valid = FALSE #then the passcode is not valid
    } else{ #otherwise, it'll be false
        valid = TRUE
    }
    data_valid <- c(data_valid, valid)
}

#the sum of all valid (TRUE) passcodes is:
sum(data_valid)

#PART II ------------
#anagrams not allowed
#let's just look at the ones that were valid from before...
t2 <- tdata[,data_valid]

#make function that rearranges letters alphabetically
alphabetize <- function(aword) { #make sure it comes in as a character
    step1 <- strsplit(aword, "") #split into string of letters
    step2 <- sort(step1[[1]]) #alphabetize
    step3 <- paste(step2, collapse = "") #put back into a word
    return(step3)
}

talpha <- apply(tdata, 1:2, alphabetize)

#now look for duplicates like before...
data_valid <- NULL
for(i in 1:ncol(talpha)){
    sub <- talpha[,i] # select column
    sub <- sub[sub != ""] # keep only words; remove empty spaces

    if(any(duplicated(sub)) == TRUE){ #if any words in sub are duplicated
        valid = FALSE #then the passcode is not valid
    } else{ #otherwise, it'll be false
        valid = TRUE
    }
    data_valid <- c(data_valid, valid)
}

#total valid passcodes:
sum(data_valid)
