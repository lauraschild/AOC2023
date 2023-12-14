#day14
rm(list = ls())
library(tidyverse)

input <- readLines("day14.txt")

parse_line <- function(line){
  strsplit(line,"") %>% 
    unlist() %>% 
    return()
}

platform <- lapply(input,
                   parse_line)

platform <- do.call(rbind, platform)

get_loadings <- function(input_col) {
  test <- platform[,input_col]
  round <- which(test == "O")
  blocked <- which(test == "#")
  #which new position will each rock have
  new_pos <- c()
  for(rock in round){
    #which is the closest sharp rock or other blockage
    proximity <- rock - blocked
    proximity <- proximity[proximity > 0]
    
    if(is_empty(proximity)){
      #rock can roll right to the front
      new_pos <- c(new_pos,1)
      blocked <- c(blocked, 1)
      next
    }
    open <- rock - min(proximity) +1
    new_pos <- c(new_pos,open)
    blocked <- c(blocked, open)
  }
  
  loadings <- (nrow(platform)+1) - new_pos
  return(sum(loadings))
}

sapply(1:ncol(platform),
       get_loadings) %>% 
  sum()

#### TASK 2 ####

roll_rocks <- function(input_col) {
  test <- platform[,input_col]
  round <- which(test == "O")
  blocked <- which(test == "#")
  #which new position will each rock have
  new_pos <- c()
  for(rock in round){
    #which is the closest sharp rock or other blockage
    proximity <- rock - blocked
    proximity <- proximity[proximity > 0]
    
    if(is_empty(proximity)){
      #rock can roll right to the front
      new_pos <- c(new_pos,1)
      blocked <- c(blocked, 1)
      next
    }
    open <- rock - min(proximity) +1
    new_pos <- c(new_pos,open)
    blocked <- c(blocked, open)
  }
  test[test == "O"] <- "."
  test[new_pos] <- "O"
  return(test)
}
platform_list <- list(platform)
for(i in 2:1000000001){
  for(direction in 1:4){
    #roll the rocks
    rolled_rocks <- lapply(1:ncol(platform),
                           roll_rocks)
    rolled_rocks <- do.call(cbind, rolled_rocks)
    
    #shift platform to new direction
    platform <- t(rolled_rocks[nrow(rolled_rocks):1,])
  }
 
  #after one whole cycle if the platform fits any of the previous platforms
  checks <- which(sapply(platform_list,
                          function(x) sum(x != platform)) == 0)
  #add it to platform list
  platform_list <- c(platform_list, list(platform))
  if(!(is_empty(checks))){
    info <- c(first = checks,
              second = i)
    break
  }
}

info

period <- diff(info)

first_step <- info[1] -1

total_cycles <- 1000000000

remainder <- (total_cycles - first_step) %% period

needed_cycles <- first_step + period + remainder

platform <- lapply(input,
                   parse_line)

platform <- do.call(rbind, platform)

for(i in 1:needed_cycles){
  for(direction in 1:4){
    #roll the rocks
    rolled_rocks <- lapply(1:ncol(platform),
                           roll_rocks)
    rolled_rocks <- do.call(cbind, rolled_rocks)
    
    #shift platform to new direction
    platform <- t(rolled_rocks[nrow(rolled_rocks):1,])
  }
}

get_loadings <- function(input_col) {
  test <- platform[,input_col]
  round <- which(test == "O")
  blocked <- which(test == "#")
  loadings <- (nrow(platform)+1) - round
  return(sum(loadings))
}
sapply(1:ncol(platform),
       get_loadings) %>% 
  sum()