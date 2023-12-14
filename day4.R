#day 4

rm(list = ls())
library(tidyverse)

input <- readLines("day4.txt")


split_into_numbers <- function(line){
  #remove card number 
  just_numbers <- unlist(strsplit(line,": ",fixed = TRUE))[-1]
  
  card_number <- parse_number(unlist(strsplit(line,": ",fixed = TRUE))[1])
  
  #split winning from card numbers
  number_list <- unlist(strsplit(just_numbers, " | ", fixed = TRUE))
  
  #separate strings into acutal vectors of numbers
  number_list <-sapply(number_list,
                       function(x) as.numeric(unlist(strsplit(x," ", fixed = TRUE)))) %>% 
    lapply(function(vector) vector[!is.na(vector)])
  
  names(number_list) <- c("winning","game")
  
  number_list <- c(number_list, card_number = card_number)
  
  return(number_list)
}

calculate_points <- function(number_list){
  winners <- sum(number_list$game %in% number_list$winning)
  
  points <- 1*2^(winners -1)
  if(winners == 0) points <- 0
  return(points)
}

copies <- data.frame(card_number = 1:length(input),
                     copy = 1)

add_copies <- function(number_list){
  winners <- sum(number_list$game %in% number_list$winning)
  
  if(winners > 0){
    which_cards <- number_list$card_number + 1:winners
    
    how_often <- copies$copy[copies$card_number == number_list$card_number]
    
    #change in copies df
    copies$copy[copies$card_number %in% which_cards] <<- copies$copy[copies$card_number %in% which_cards] + how_often
      
  }
}

solve_a_card <- function(line){
  split_into_numbers(line) %>% 
    calculate_points() %>% 
    return()
}

edit_copies <- function(line){
  split_into_numbers(line) %>% 
    add_copies()
}

#task 1
sapply(input,
       solve_a_card) %>% 
  unname() %>% 
  sum()


#task 2
shh <- sapply(input,
       edit_copies)

sum(copies$copy)