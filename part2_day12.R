rm(list = ls())
library(tidyverse)
library(memoise)

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


#function to find solutions
find_solutions <- function(springs,
                           sizes,
                           group_size = 0){
  #return 1 if there are no more group sizes and there is no current group
  if(is.na(springs[1])){
    if(is.na(sizes[1]) & group_size == 0){
      return(1)
    }else{
      return(0)
    }
  }
  
  num_solutions <- 0
  if(springs[1]== "?"){
    symbol <- c(".","#")
  }else{
    symbol <- springs[1]
  }
  
  for(sym in symbol){
    #expand the group if it's #
    if(sym == "#"){
      num_solutions <- num_solutions + mem_find_solutions(springs[2:length(springs)],
                                                          sizes,
                                                          group_size +1)
    }else{
      if(group_size > 0){
        if(!is.na(sizes[1])& sizes[1] == group_size){
          num_solutions <- num_solutions + mem_find_solutions(springs[2:length(springs)],
                                                              sizes[2:length(sizes)])
        }
      }else{ # if . is at the end *of a group and it doesn't match the first size continue without removing a group
        num_solutions <- num_solutions + mem_find_solutions(springs[2:length(springs)],
                                                            sizes)
      }
    }
  }
  return(num_solutions)
  
}

mem_find_solutions <- memoise(find_solutions)

counts <- c()
for(i in 1:length(records)){
  springs <- records[[i]][[1]]
  springs <- rep(c(springs,"?"),5)
  springs <- c(springs[-length(springs)],".")
  
  sizes <- records[[i]][[2]]
  sizes <- rep(sizes,5)
  counts <- c(counts,(mem_find_solutions(springs,
                           sizes)))
  forget(mem_find_solutions)
}
counts <- sum(counts)

#solution
sprintf("%.0f",counts)