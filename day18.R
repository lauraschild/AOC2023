#day18
rm(list = ls())
library(tidyverse)
library(sf)

input <- readLines("day18.txt")

parse_line <- function(line) {
  df <- unlist(strsplit(line," ")) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate(V3 = gsub("[(]","",V3),
           V3 = gsub("[)]","",V3)) %>% 
    rename(direction = V1,
           steps = V2,
           color = V3)
  direction <- tail(unlist(strsplit(df$color,"")),1)
  translate <- c("R","D","L","U")
  orig <- c("0","1","2","3")
  new_direction <- translate[match(direction,orig)]
  new_steps <- substr(df$color,2,6)
  new_steps <- as.numeric(as.hexmode(new_steps))
  cbind(df,
        data.frame(new_direction,new_steps)) %>% 
    return()
}

trench_guide <- lapply(input,
                       parse_line) %>% 
  bind_rows()

trench_coords <- data.frame(row = 0,
                            col = 0)
for(i in 1:nrow(trench_guide)){
  current_coord <- trench_coords[i,]
  
  direction <- trench_guide$direction[i]
  steps <- as.numeric(trench_guide$steps[i])
  
  if(direction == "R"){
    next_coord <- current_coord + c(0,steps)
  }
  if(direction == "L"){
    next_coord <- current_coord - c(0,steps)
  }
  if(direction == "U"){
    next_coord <- current_coord + c(steps,0)
  }
  if(direction == "D"){
    next_coord <- current_coord - c(steps,0)
  }
  
  trench_coords <- rbind(trench_coords,
                         next_coord)
}

head(trench_coords)
plot(row~col,trench_coords,type = "l")

poly <- trench_coords %>%
  st_as_sf(coords = c("col","row")) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

sprintf("%.0f",sum(as.numeric(trench_guide$steps))/2 + st_area(poly) +1)

#### PART 2
trench_coords <- data.frame(row = 0,
                            col = 0)
for(i in 1:nrow(trench_guide)){
  current_coord <- trench_coords[i,]
  
  direction <- trench_guide$new_direction[i]
  steps <- as.numeric(trench_guide$new_steps[i])
  
  if(direction == "R"){
    next_coord <- current_coord + c(0,steps)
  }
  if(direction == "L"){
    next_coord <- current_coord - c(0,steps)
  }
  if(direction == "U"){
    next_coord <- current_coord + c(steps,0)
  }
  if(direction == "D"){
    next_coord <- current_coord - c(steps,0)
  }
  
  trench_coords <- rbind(trench_coords,
                         next_coord)
}

head(trench_coords)
plot(row~col,trench_coords,type = "l")

poly <- trench_coords %>%
  st_as_sf(coords = c("col","row")) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

sprintf("%.0f",sum(as.numeric(trench_guide$new_steps))/2 + st_area(poly) +1)



