#day 10

rm(list = ls())
library(tidyverse)

input <- readLines("day10.txt")

parse_line <- function(line){
  old <- c("|",".","-","F","L","J","7","S")
  new <- c("NS",NA,"WE","SE","NE","NW","SW","start")
  
  pipes <- strsplit(line,"") %>% 
    unlist()
  
  new[match(pipes,old)] %>% 
    return()
}

pipes <- lapply(input,
                parse_line) 

pipes <- do.call(rbind,pipes)


starting_point <- which(pipes == "start",
                        arr.ind  = TRUE)

#function to translate a connection vector to new coordinate
new_coord <- function(connection,
                      old_coord){
  row <- old_coord[1]
  col <- old_coord[2]
  
  if(connection %in% c("E","W")){
    row <- row
    
    col <- ifelse(connection == "E",
                  col +1,
                  col -1)
  }
  if(connection %in% c("N","S")){
    col <- col
    
    row <- ifelse(connection == "S",
                  row +1,
                  row -1)
  }
  return(c(row,col))
}

new_connection <- function(connection,
                           new_tile){
  new_connections <- pipes[new_tile[1],new_tile[2]]
  if(is.na(new_connections)){
    return(NA)
  }
  new_connections <- unlist(strsplit(new_connections,""))
  #translate connection to needed pipe
  present_connection <- c("N","S","E","W")
  connection_needed <- c("S","N","W","E")
  
  connection_needed <- connection_needed[match(connection,present_connection)]
  
  if(sum(connection_needed %in% new_connections) == 0){
    return(NA)
  }
  
  new_connection <- new_connections[new_connections != connection_needed]
  return(new_connection)
}

#test out different pipes for the starting position
for(start_connection in c("N","S","E","W")){
  connection <- start_connection
  current_tile <- starting_point
  start_reached <- FALSE
  steps <- 0
  positions <- data.frame(row = current_tile[1],
                          col = current_tile[2])
  outside_inside <- data.frame(row = numeric(),
                               col = numeric(),
                               type = character())
  
  while(!start_reached){
    steps <- steps +1
    #move to next tile
    new_tile <- new_coord(connection,
                          current_tile)
    in_between <- current_tile + 0.5*(new_tile - current_tile)
    positions <- rbind(positions,in_between)
    
    positions <- rbind(positions, new_tile)
    #check if the next tile is the start
    if(sum(new_tile == starting_point) == 2){
      print(steps)
      true_positions <- positions
      break
    }
    
    #if no check if movement is possible
    connection <- new_connection(connection,
                                 new_tile)
    if(is.na(connection)){
      break
    }
    
    
    current_tile <- new_tile
  }
}



#plot(raster(t(as.numeric(test_pipes))))
#go through  all NA cells and check if path is in all ways

ggplot(true_positions,
       aes(col,
           row))+
  geom_line()


polygon <-true_positions %>% 
  st_as_sf(coords = c("col","row")) %>% 
  summarize(geometry = st_combine(geometry)) %>% 
  st_cast("POLYGON") 



new_positions <- true_positions %>% 
  mutate(row = row*2,
         col = col*2)

#fill matrix with extended pipes
new_pipes <- matrix(data = 0,
                    nrow = 2*nrow(pipes),
                    ncol = 2*ncol(pipes))

new_pipes[as.matrix(new_positions)] <- 1


#check all zeroes if they lies within the polygon
all_zeros <- which(new_pipes == 0, arr.ind = TRUE)

#function to check if an outside cell is within reach
outside_check <- function(current_zero){
  end_row <- nrow(new_pipes)
  end_col <- ncol(new_pipes)
  
  south <- 1 %in% new_pipes[current_zero[1]:end_row,current_zero[2]]
  north <- 1 %in% new_pipes[1:current_zero[1],current_zero[2]]
  east <- 1 %in% new_pipes[current_zero[1],current_zero[2]:end_col]
  west <- 1 %in% new_pipes[current_zero[1], 1:current_zero[2]]
  outside <- ifelse(sum(south,north,east,west) < 4,
                    TRUE,
                    FALSE) 
  
  return(outside)
}

#function to try and find a way outside

added_outside <- new_pipes
for(i in 1:nrow(all_zeros)){

  current_zero <- all_zeros[i,]
  
  outside <- outside_check(current_zero)
  
  if(outside){
    added_outside[current_zero[1],current_zero[2]] <- 2
  }
  
}
outside_touching <- function(current_zero){
  end_row <- nrow(added_outside)
  end_col <- ncol(added_outside)
  
  north <- added_outside[current_zero[1]:1,current_zero[2]]
  south <- added_outside[current_zero[1]:end_row,current_zero[2]]
  east <- added_outside[current_zero[1],current_zero[2]:end_col]
  west <- added_outside[current_zero[1],current_zero[2]:1]
  
  outside_near <- function(vector){
    outside_present <- 2 %in% vector
    if(!outside_present){
      return(FALSE)
    }
    outside_position <- which(vector == 2)[1]
    close_ones <- sum(vector[1:(outside_position -1)])
    
    if(close_ones == 0){
      return(TRUE)
    }
    
    return(FALSE)
  }
  
  outside_in_any_direction <-   lapply(list(north,east,south,west),
                                       outside_near) %>% 
    unlist() %>% 
    sum()
  
  if(outside_in_any_direction > 0){
    return(TRUE)
  }
  
  return(FALSE)
}
old_zeros <- length(added_outside)
still_converting <- TRUE
while(still_converting){
  remaining_zeros <- which(added_outside == 0, arr.ind = TRUE)
  if(length(remaining_zeros) == length(old_zeros)){
    still_converting <- FALSE
  } 
  old_zeros <- remaining_zeros
  for(i in 1:nrow(remaining_zeros)){
    current_zero <- remaining_zeros[i,]
    close_to_outside <- outside_touching(current_zero)
    
    if(close_to_outside){
      added_outside[current_zero[1], current_zero[2]] <- 2
    }
  }
}

last_zeros <- which(added_outside == 0, arr.ind = TRUE)

as.data.frame(last_zeros) %>% 
  mutate(row_mod = row %% 2,
         col_mod = col %% 2) %>% 
  filter(col_mod == 0, row_mod == 0) %>% 
  nrow()
