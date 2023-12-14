#day2
library(tidyverse)
#12 red
#13 green
#14 blue

input <- readLines("day2.txt")

#make individual game dfs
make_df <- function(game){
  string <- input[game]
  
  #get only the draws
  all_draws <-  unlist(strsplit(string,": "))[2]
  
  #get seperate draws
  separate_draws <- unlist(strsplit(all_draws,"; "))
  
  df <- lapply(separate_draws,
                 function(x) unlist(strsplit(x, ", "))) %>% 
    unlist() %>% 
    lapply(function(x)unlist(strsplit(x," "))) %>% 
    unlist() %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame()
  
  names(df) <- c("count","color")
  df$count <- as.numeric(df$count)
  
  #are there red violations
  red_check <- sum(df$count[df$color == "red"] >12)>0
  #are there green violations
  green_check <- sum(df$count[df$color == "green"]>13)>0
  #are there blue violations
  blue_check <- sum(df$count[df$color == "blue"]>14)>0
  
  if(sum(red_check,blue_check,green_check) == 0){
    return(game)
    }else{return(NA)}
}

good_games <- sapply(1:length(input),
                     make_df)

#solution part 1
sum(good_games, na.rm = TRUE)

#### PART 2####

#make individual game dfs
agathe_power <- function(game){
  string <- input[game]
  
  #get only the draws
  all_draws <-  unlist(strsplit(string,": "))[2]
  
  #get seperate draws
  separate_draws <- unlist(strsplit(all_draws,"; "))
  
  df <- lapply(separate_draws,
               function(x) unlist(strsplit(x, ", "))) %>% 
    unlist() %>% 
    lapply(function(x)unlist(strsplit(x," "))) %>% 
    unlist() %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame()
  
  names(df) <- c("count","color")
  df$count <- as.numeric(df$count)
  
  #are there red violations
  min_red <- max(df$count[df$color=="red"])
  #are there green violations
  min_green <- max(df$count[df$color == "green"])
  #are there blue violations
  min_blue <- max(df$count[df$color == "blue"])
  
  return(min_red*min_green*min_blue)
}

powers <- sapply(1:length(input),
                 agathe_power)
#solution part 2
sum(powers)
