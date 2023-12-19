#day17
rm(list = ls())
library(tidyverse)

input <- readLines("day17.txt")
map <- lapply(input,
              function(x) unlist(strsplit(x,"")))
map <- do.call(rbind,map)

#data frame with points distances and straight steps
d_frame <- data.frame(point_ID = 1:length(map),
                      expand.grid(row = 1:nrow(map),col = 1:ncol(map)),
                      weight = c(map),
                      cost = Inf,
                      previous_point = NA)

d_frame <-expand_grid(d_frame,
                      direction = c("N","E","S","W"),
                      steps = c(1:3))
d_frame <- rbind(data.frame(point_ID = 1,
                            row = 1,
                            col = 1,
                            weight = 3,
                            cost = 0,
                            previous_point = NA,
                            direction = "forward",
                            steps = 0),
                 d_frame) %>% 
  filter(!(row ==1 & direction == "S"),
         !(row == nrow(map) & direction == "N"),
         !(col == 1 & direction == "E"),
         !(col == ncol(map) & direction == "W")) %>% 
  rowid_to_column("check_ID")

#list with unvisited points (point_id s)
unvisited <- d_frame$check_ID
#list with visited points
visited <- c()

#add first step
check <- 1
while(length(unvisited > 0)){

  #get current point position
  row <- d_frame$row[d_frame$check_ID == check]
  col <- d_frame$col[d_frame$check_ID == check] 
  cost <- d_frame$cost[d_frame$check_ID == check]
  current_direction <- d_frame$direction[d_frame$check_ID == check]
  current_steps <- d_frame$steps[d_frame$check_ID == check]
  point <- d_frame$point_ID[d_frame$check_ID == check]
  
  if(point == length(map)){
    break
  }
  
  #adjacent point id
  adjacent <- c(W = point - nrow(map),
                N = point -1, 
                S = point +1, 
                E =point + nrow(map))
  
  #exlcude adjacents if edge cases
  exclude <- 5
  if(row == 1){
    if(col == 1){
      exclude <- c(1,2)
    }else if(col == ncol(map)){
       exclude <- c(2,4) 
      }else{
      exclude <- 2
    }
  }else if(col == 1){
    if(row == nrow(map)){
      exclude <- c(1,3)
    }else{
      exclude <- 1
    }
  }else if(col == ncol(map)){
    if(row == nrow(map)){
      exclude <- c(3,4)
    }else{
      exclude <- 4
    }
  }else if(row == nrow(map)){
    exclude <- 3
  }
  
  adjacent <- adjacent[-exclude]
  
  #exclude opposite direction (can't go back)
  translate <- c("S","W","N","E")
  original <- c("N","E","S","W")
  opposite <- translate[match(current_direction,original)]
  if(current_direction != "forward"){
    adjacent <- adjacent[names(adjacent) != opposite]
  }
  #exclude if current direction is already at three and present in adjacent
  if(current_steps == 3){
    adjacent <- adjacent[names(adjacent) != current_direction]
  }
  
  #loop through adjacents
  for(i  in 1:length(adjacent)){
    if(is_empty(adjacent)){
      next
    }
    next_step <- adjacent[i]
    #add weight to current cost
    weight <- d_frame %>% 
      filter(point_ID == next_step) %>% 
      pull(weight) %>% 
      unique() %>% 
      as.numeric()
    new_cost <- weight + cost
    
    #get number of steps and direction
    new_direction <- names(next_step)
    
    new_steps <- 1
    if(new_direction == current_direction){
      new_steps <- current_steps + 1
    }
    
    #check if this is smaller than current cost for step
    current_cost <- d_frame %>% 
      filter(point_ID == next_step & direction == new_direction,
             steps == new_steps) %>% 
      pull(cost)
  
    if(current_cost < new_cost){
      next
    }
    
    #get check ID for row to be changed
    change <- d_frame %>% 
      filter(point_ID == next_step & direction == new_direction,
             steps == new_steps) %>% 
      pull(check_ID)
    #edit d_frame 
    d_frame <- d_frame %>% 
      mutate(cost = ifelse(check_ID == change,
                           new_cost,
                           cost),
             previous_point = ifelse(check_ID == change,
                                     check,
                                     previous_point))
    
  }
  
  visited <- c(visited, check)
  
  unvisited <- unvisited[!(unvisited %in% visited)]
  if(is_empty(unvisited)){
    break
  }
  #chose cheapest unvisited point next
  check <- d_frame %>% 
    filter(check_ID %in% unvisited) %>% 
    filter(cost == min(cost)) %>% 
    sample_n(1) %>% 
    pull(check_ID)
  
}

#evaluate d_frame
d_frame %>% 
  filter(point_ID == length(map)) %>% 
  filter(cost == min(cost))


# paths <- c(1873)
# current <- 1873
# previous <- 3
# while(previous != 1){
#   previous <- d_frame$previous_point[d_frame$check_ID == current]
#   paths <- c(paths, previous)
#   current <- previous
# }
# # path <- c(169,156,157,158,145,132,131,118,105,106,93,80,79,66,41,53,54,1,14,28,27)
# d_frame %>%
#   filter(check_ID %in% paths) %>%
#   ggplot(aes(col,
#              row,
#              col = weight))+
#   geom_point()+
#   scale_y_reverse()+
#   geom_text(aes(label =weight))
