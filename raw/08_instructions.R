test <- read.csv(here("08_testinput.csv"), header = FALSE)
puzzle <- read.csv(here("08_input.csv"), header = FALSE)

input <- puzzle
input_sep <- input %>% 
    separate(V1, into = c("action", "condition"), sep = " if ") %>% 
    separate(action, into = c("var", "action", "val"), sep = " ")

paste_special <- function(var, action, val) {
    if(action == "inc"){
        return(paste("df$", var, "+", val, sep = ""))
    } else {
        return(paste("df$", var, "-", val, sep = ""))
    }
}

eval_string <- function(string) {
    eval(parse(text = string))
}

#parse out variables...
names <- input_sep %>% as.tibble() %>% 
    separate(condition, into = c("var2", "sign", "val2"), sep = " ") %>% 
    gather(c(var, var2), key=var, value=names) %>% 
    distinct(names) %>% 
    pull(names)
    
df <- data.frame(value = rep(0, length(names)))
df <- as.data.frame(t(df))
colnames(df) = as.vector(names)
big_ans = 0

for(i in 1:nrow(input_sep)){
    var <- input_sep$var[i]
    action <- input_sep$action[i]
    val <- input_sep$val[i]
    condition <- paste("df$", input_sep$condition[i], sep = "")
    
    if(eval_string(condition)){ #if this condition is true..
        ans <- eval_string(paste_special(var, action, val))
        df[var] <- ans
        print(ans)
        if(ans > big_ans){
            big_ans <- ans
        }
    }
}
print(df)
max(df)
print(big_ans)
