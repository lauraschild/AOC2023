#day13
rm(list = ls())

library(tidyverse)

input <- readLines("day13.txt")

splits <- c(0,which(input == ""),length(input)+1)

parse_patterns <- function(i){
  start <- splits[i]+1
  end <- splits[i+1]-1
  do.call(rbind,strsplit(input[start:end],""))
}

patterns <- lapply(1:(length(splits)-1),
                   parse_patterns)

find_axes <- function(pattern){
  vertical_axes <- 1:(ncol(pattern)-1)
  horizontal_axes <- 1:(nrow(pattern)-1)
  
  #check vertical axes
  for(axis in vertical_axes){
    cols_needed <- min(ncol(pattern)-axis,axis)
    left <- pattern[,(axis-cols_needed+1):axis]    
    right <- pattern[,(axis+1):(axis+cols_needed)]
    
    if(cols_needed == 1){
      left <- matrix(left)
      right <- matrix(right)
      }
    #mirror the left one
    mirror_check <- left[,ncol(left):1] != right
    
    if(sum(mirror_check) == 1){
      vertical <- axis
      break
    }
    
    vertical <- 0
  }
  
  for(axis in horizontal_axes){
    rows_needed <- min(nrow(pattern)-axis,axis)
    
    upper <- pattern[(axis-rows_needed+1):axis,]
    lower <- pattern[(axis+1):(axis+rows_needed),]
    
    if(rows_needed == 1){
      mirror_check <- upper != lower
    }else{
      mirror_check <- upper[nrow(upper):1,] != lower
    }
    
    if(sum(mirror_check) == 1){
      horizontal <- axis
      break
    }
    horizontal <- 0
  }
  
  result <- vertical + horizontal*100
  
  return(result)
}

all <- sapply(patterns,
              find_axes)
