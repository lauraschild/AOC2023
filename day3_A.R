#day3 

library(tidyverse)

input <- readLines("day3.txt")

seperate_nicely <- function(line){
  #replace numbers
  gsub("[0-9]",replacement = "A",line)
  sep <- unlist(strsplit(line,""))
  better_split <- c()
  for(position in 1:length(sep)){
    #check if numeric
    #check if is NA
    if(is.na(sep[position])){
      next
    } 
    numeric_check <- grepl("[0-9]",sep[position])
    if(!numeric_check){
      better_split <- c(better_split,sep[position])
      next
    }
    next_numerics <- grepl("[0-9]",sep[position:(position + 5)])
    
    last_numeric <- 0
    end_found <- FALSE
    i <- 1
    while(!end_found){
      if(next_numerics[i]==TRUE){
        last_numeric <- i-1
        i <- i+1
      }else{
        end_found <- TRUE
      }
    }
    
    #set actual position to complete numeric
    number <- paste0(sep[position:(position+last_numeric)],
                     collapse = "")
    
    better_split <- c(better_split,number)
    
    #set all other numerics to NAs
    sep[(position+1):(position+last_numeric)] <- NA
  }
  
  return(better_split)
}


split_lines <- function(line){
  sep <- unlist(strsplit(line,""))
  return(sep)
}

all_lines <- lapply(input,
                    split_lines) 
manual <- do.call(rbind, all_lines)


positions <- expand.grid(col = 1:ncol(manual),
                         row = 1:nrow(manual))
#check_function for numbers
get_engine_parts <- function(position){
  print(position)
  row <- positions$row[position]
  col <- positions$col[position]
  value <- manual[row,col]
  
  #return nothing if NA
  is_na <- is.na(value)
  if(is_na){
    return()
  }
  #return nothing if not numeric
  is_numeric <- grepl("[0-9]",value)
  if(!is_numeric){
    return()
  }
  
  #check for symbols in vicinity if numeric
  vicinity_col <- ifelse(col+1>ncol(manual),
                         ncol(manual),
                         col +1)
  vicinity_row <- ifelse(row+1 > nrow(manual),
                         nrow(manual),
                         row +1)
  vicinity <- manual[(row-1):vicinity_row,(col-1):vicinity_col]
  numeric_check <- grepl("[0-9]",vicinity)
  point_check <- grepl("[.]",vicinity)
  
  symbol_check <- !(point_check + numeric_check)
  
  if(sum(symbol_check) == 0){
    return()
  }
  
  #get part number if symbol was found
  end_col <- ifelse(col+5 > ncol(manual),
                    ncol(manual),
                    col +5)
  next_numerics <- grepl("[0-9]",manual[row,col:end_col])
  
  last_numeric <- 0
  end_found <- FALSE
  i <- 1
  while(!end_found){
    if(next_numerics[i]==TRUE){
      last_numeric <- i-1
      i <- i+1
      if(i > length(next_numerics)){
        end_found <- TRUE
      }
    }else{
      end_found <- TRUE
    }
  }
  
  start_col <- ifelse(col -5 < 1,
                      1,
                      col -5)
  previous_numerics <- grepl("[0-9]",manual[row,start_col:col])
  
  first_numeric <- 0
  start_found <- FALSE
  i <- length(previous_numerics)
  while(!start_found){
    if(previous_numerics[i]==TRUE){
      first_numeric <- i-length(previous_numerics)
      i <- i-1
      if(i == 0){
        start_found <- TRUE
      }
    }else{
      start_found <- TRUE
    }
  }
  #get part number
  part_number <- as.numeric(paste0(manual[row,(col+first_numeric):(col+last_numeric)],
                                   collapse = ""))
  
  #set other numbers to NA to skip next time
  manual[row,(col+first_numeric):(col+last_numeric)] <<- NA
  
  return(part_number)
}

parts <- sapply(1:nrow(positions),
                get_engine_parts)

sum(unlist(parts))


