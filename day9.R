#day9
rm(list = ls())
library(tidyverse)

input <- readLines("day9.txt")

#prepare input
#function to use on each line
parse_line <- function(line){
  strsplit(line," ") %>% 
    unlist() %>% 
    parse_number() %>% 
    return()
}

OASIS <- lapply(input,
                parse_line) 
OASIS <- do.call(cbind,OASIS)

extrapolate <- function(ts,
                        task2 = FALSE){
  last_digit <- tail(ts,1)
  first_digit <- ts[1]
  zero_diff <- FALSE
  
  while(!zero_diff){
    ts <- diff(ts)
    zero_diff <- sum(ts) == 0 & 0 %in% ts & length(unique(ts)) == 1
    last_digit <- c(last_digit,tail(ts,1))
    first_digit <- c(first_digit, ts[1])
  }
  if(task2){
    for(i in length(last_digit):2){
      current <- first_digit[i]
      upper <- first_digit[i-1]
      first_digit[i-1] <- upper - current
    }
    return(first_digit[1])
  }
  sum(last_digit) %>% 
    return()
}

sapply(1:ncol(OASIS),
       function(m) extrapolate(OASIS[,m])) %>% 
  sum()

sapply(1:ncol(OASIS),
       function(m) extrapolate(OASIS[,m],TRUE)) %>% 
  sum()
