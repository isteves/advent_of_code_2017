#puzzle 09 garbage
library(stringr)
library(here)

#rules:
# <>, empty garbage.
# <random characters>, garbage containing random characters.
# <<<<>, because the extra < are ignored.
# <{!>}>, because the first > is canceled.
# <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
# <!!!>>, because the second ! and the first > are canceled.
# <{o"i!a,<{i<a>, which ends at the first >.

#chartr(old, new, x)

#test strings
# string <- "{{<!>},{<!>},{<!>},{<a>}}"

#puzzle string
string <- read.table(here("09_data.txt"), stringsAsFactors = FALSE)
string <- string[[1]]
cat("Starting # characters:", nchar(string))

#removing !
while(str_detect(string, "!") == TRUE){
    string <- str_replace(string, "!\\D", "")
}
cat("After removing !:", nchar(string))

#removing garbage
n_garbage <- NULL
while(str_detect(string, ">") == TRUE){
    garbage_start <- str_locate(string, "<")[1]
    garbage_end <- str_locate(string, ">")[1]
    n_garbage <- c(n_garbage,
                   nchar(str_sub(string, garbage_start, garbage_end)) - 2)
    str_sub(string, garbage_start, garbage_end) <- ""
}
cat("Garbage:", sum(n_garbage))

#counting {}
#count { based on position; for every } encountered, minus 1
locs <- str_locate_all(string, "\\{")[[1]]
points <- NULL
#for every {
for(i in 1:nrow(locs)){
    pospoints <- str_count(substr(string, 1, locs[i]), "\\{")
    negpoints <- str_count(substr(string, 1, locs[i]), "\\}")
    points <- c(points, pospoints - negpoints)
}
print(sum(points))

#how many letters in garbage?
#added n_char above..
