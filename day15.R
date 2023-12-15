#day 15
rm(list = ls())
library(tidyverse)

init_sequence <- read.csv("day15.txt",
                          header = FALSE)
values <- c()

for(m in 1:ncol(init_sequence)){
  integers <- utf8ToInt(init_sequence[,m])
  
  hash_algo <- function(i){
    if(i == 1){
      current_value <- (integers[i]*17) %% 256
      return(current_value)
    }
    current_value <- hash_algo(i-1) + integers[i]
    current_value <- (current_value*17) %% 256
    return(current_value)
  }
  
  values <- c(values, hash_algo(length(integers)))
}

message(paste0("Solution Part 1: ",sum(values)))

#### PART 2 ####
boxes <- rep(list(data.frame(label = character(),
                             lense = numeric())),
             256)
for(m in 1:ncol(init_sequence)){
  init <- init_sequence[,m]
  #which operation
  add <- grepl("=",init)
  if(add){
    lense_focus <- as.numeric(tail(unlist(strsplit(init,"")),1))
  }
  letters <- unlist(strsplit(init,""))
  letters <- paste0(letters[grepl("[a-z]",letters)],collapse ="")
  
  integers <- utf8ToInt(letters)
  
  box <- hash_algo(length(integers))
  df <- boxes [[box+1]]
  
  if(add){
    
    if(letters %in% df$label){
      df$lense[df$label == letters] <- lense_focus
    }else{
      df <- df %>% 
        rbind(data.frame(label = letters,
                         lense = lense_focus))
    }
    
    boxes[[box+1]] <- df
    next
  }
  df <- df %>%
    filter(label != letters)
  
  boxes[[box+1]] <- df
}

result <- lapply(1:256,
                 function(x) rowid_to_column(mutate(boxes[[x]],box = x))) %>% 
  bind_rows() %>% 
  mutate(power = box*rowid*lense) %>% 
  pull(power) %>% 
  sum()

message(paste0("Solution Part 2: ",result))

  