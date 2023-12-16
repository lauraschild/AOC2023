#day 16
rm(list = ls())
library(tidyverse)

input <- readLines("day16.txt")

grid <-lapply(input,
              function(x) unlist(strsplit(x,"")))

grid <- do.call(rbind,grid)

configs <- data.frame(row = 1,
                      col = 1:ncol(grid),
                      direction = 3) %>% 
      rbind(data.frame(row = nrow(grid),
                       col = 1:ncol(grid),
                       direction = 1)) %>% 
  rbind(data.frame(row = 1:nrow(grid),
                   col = 1,
                   direction = 2)) %>% 
  rbind(data.frame(row = 1:nrow(grid),
                   col = ncol(grid),
                   direction = 4))

get_energy <- function(config_row = 1,
                       task1){
  previous_checks <- data.frame(row = numeric(),
                                col = numeric(),
                                directions = numeric())
  i = 0
  new_starting_points <- list()
  start_point <- c(configs$row[config_row],configs$col[config_row])
  direction <- configs$direction[config_row]
  if(task1){
    start_point <- c(1,1)
    direction <- 2
  }
  energy_grid <- grid
  energy_grid[energy_grid != 1] <- 1
  more_to_check <- TRUE
  while(more_to_check){
    #check where we have an obstacle including the current starting point
    if(direction == 2){
      possible_path <- grid[start_point[1],start_point[2]:ncol(grid)]
    }else if(direction == 3){
      possible_path <- grid[start_point[1]:nrow(grid),start_point[2]]
    }else if(direction == 4){
      possible_path <- grid[start_point[1],start_point[2]:1]
    }else if(direction == 1){
      possible_path <- grid[start_point[1]:1,start_point[2]]
    }
    grid_ends <- FALSE
    next_obstacle <- min(which(possible_path != "."))-1
    if(next_obstacle == Inf){
      grid_ends <- TRUE
      next_obstacle <- length(possible_path)-1
    }
    #fill all cells inbetween as energized
    #move on to check position
    if(direction == 1){
      energy_grid[(start_point[1]-next_obstacle):start_point[1],start_point[2]] <- 0
      check_point <- start_point - c(next_obstacle,0)
    }else if(direction == 2){
      energy_grid[start_point[1],start_point[2]:(start_point[2]+next_obstacle)] <- 0
      check_point <- start_point + c(0, next_obstacle)
    }else if(direction == 3){
      energy_grid[start_point[1]:(start_point[1]+next_obstacle),start_point[2]] <- 0
      check_point <- start_point + c(next_obstacle,0)
    }else if(direction == 4){
      energy_grid[start_point[1],(start_point[2]-next_obstacle):start_point[2]] <- 0
      check_point <- start_point - c(0,next_obstacle)
    }
    obstacle <- possible_path[next_obstacle+1]
    
    #have we been at this exact position with this direction before?
    rerun <- previous_checks %>% 
      filter(row == check_point[1], col == check_point[2],
             directions == direction) %>% 
      nrow()>0
    if(rerun|grid_ends){
      if(i == length(new_starting_points)){
        #break loop if we've been through all starting positions
        more_to_check <- FALSE
        break
      }
      #move on to next starting point if more are available
      i <- i +1
      start_point <- new_starting_points[[i]]
      direction <- start_point[3]
      start_point <- start_point[1:2]
    }else if (!rerun){#continue at check point if it's not a rerun
      #translate the direction depending on the obstacle
      #add this check to previous checks
      previous_checks <- rbind(previous_checks,
                               data.frame(row = check_point[1],col = check_point[2], directions = direction))
      if(obstacle == "\\"){
        if(direction %in% c(1,3)){
          new_direction <- 5 - direction 
        }else{
          new_direction <- direction +1
          if(new_direction == 5){
            new_direction <- 1
          }
        }
      }
      if(obstacle == "/"){
        if(direction %in% c(1,3)){
          new_direction <- direction +1
        }else{
          new_direction <- direction -1
        }
      }
      if(obstacle == "-"){
        if(direction %in% c(1,3)){
          new_direction <- c(2,4)
        }else{
          new_direction <- direction
        }
      }
      if(obstacle == "|"){
        if(direction %in% c(2,4)){
          new_direction <- c(1,3)
        }else{
          new_direction <- direction
        }
      }
      
      if(length(new_direction) > 1){
        if(new_direction[1] == 1){
          new_start_point <- check_point + c(-1,0)
        }
        if(new_direction[1] == 2){
          new_start_point <- check_point + c(0,1)
        }
        if(new_direction[1] == 3){
          new_start_point <- check_point + c(1,0)
        }
        if(new_direction[1] == 4){
          new_start_point <- check_point + c(0,-1)
        }
        
        if((!0 %in% new_start_point) & new_start_point[1] <= nrow(grid) & new_start_point[2] <= ncol(grid)){
          new_starting_points <- c(new_starting_points,
                                   list(c(new_start_point, new_direction[1])))
        }
        new_direction <- new_direction[2]
      }
      
      #translate direction if no 
      if(new_direction == 1){
        new_start_point <- check_point + c(-1,0)
      }
      if(new_direction == 2){
        new_start_point <- check_point + c(0,1)
      }
      if(new_direction == 3){
        new_start_point <- check_point + c(1,0)
      }
      if(new_direction == 4){
        new_start_point <- check_point + c(0,-1)
      }
      start_point <- new_start_point
      direction <- new_direction
      
      if(0 %in% start_point | start_point[2] > ncol(grid)|start_point[1] > nrow(grid)){
        if(i == length(new_starting_points)){
          #break loop if we've been through all starting positions
          more_to_check <- FALSE
          break
        }
        #move on to next starting point if more are available
        i <- i +1
        start_point <- new_starting_points[[i]]
        direction <- start_point[3]
        start_point <- start_point[1:2]
      }
    }
  }
  
  sum(energy_grid == 0) %>% 
    return()
  
}

get_energy(task1 = TRUE)

sapply(1:nrow(configs),
       get_energy,
       task1 = FALSE) %>% 
  max()