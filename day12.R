#day 12

rm(list = ls())
library(tidyverse)

input <- readLines("day12.txt")

parse_line <- function(line){
  elements <- unlist(strsplit(line, " "))
  counts <- as.numeric(unlist(strsplit(elements[2],",")))
  conditions <- unlist(strsplit(elements[1],""))
  list(conditions,
       counts) %>% 
    return()
}

records <- lapply(input,
                  parse_line)

fill_all_possible <- function(conditions,
                              counts,
                              task2){
  unknowns <- which(conditions == "?")
  known <- which(conditions == "#")
  total_damaged <- sum(counts)
  present_damaged <- sum(conditions == "#")
  needed_damaged <- total_damaged - present_damaged
  
  all_possibilities <- t(combn(unknowns,needed_damaged))
  
  if(task2){
    test <- all_possibilities %>% 
      as.data.frame() %>% 
      cbind(t(known)) %>% 
      as.matrix()
    
    breaks <- lapply(1:nrow(test),
                     function(x) sum(diff(sort(x))>1))

    all_possibilities <- all_possibilities[breaks == (length(counts)-1)]
  }
  
  change_conditions <- function(index){
    indices <- all_possibilities[index,]
    new_conditions <- conditions
    new_conditions[indices] <- "#"
    new_conditions[new_conditions != "#"] <- "."
    return(new_conditions)
  }
  
  lapply(1:nrow(all_possibilities),
         change_conditions) %>% 
    return()
}

check_fills <- function(possibility,
                        counts){
  
  #go through possibility and count damaged ones
  possibility <- c(".",possibility,".")
  differences <- diff(which(possibility == "."))-1
  differences <- differences[differences != 0]
  if(length(differences) != length(counts)){
    return(FALSE)
  }
  check <- sum(differences == counts) == length(counts)
  return(check)
}

evaluate_arrangements <- function(record,
                                  task2 = FALSE){
  conditions <- record[[1]]
  counts <- record [[2]]
  if(task2){
    conditions <- rep(c(conditions,"?"),5)
    conditions <- conditions[-length(conditions)]
    counts <- rep(counts,5)
  }
  #fill conditions in all possible ways
  all_possibilities <- fill_all_possible(conditions,
                                        counts,
                                        task2 = task2)
  #check which ones work
  checks <- sapply(all_possibilities,
                   check_fills,
                   counts)
  #return number of working arrangements
  return(sum(checks))
}

sapply(records,
       evaluate_arrangements) %>% 
  sum()


