#day 6
rm(list = ls())
library(tidyverse)

input <- readLines("day6.txt")
time <- parse_number(unlist(strsplit(input[1]," ")))
time <- time[!is.na(time)]
distance <- parse_number(unlist(strsplit(input[2]," ")))
distance <- distance[!is.na(distance)]

get_possible_distance <- function(charging_time,
                                  total_time){
  travel_time <- total_time - charging_time
  speed <- charging_time
  travel_distance <- travel_time * speed
  return(travel_distance)
}

distance_vector <- function(i){
  total_time <- time[i]
  winning_distance <- distance[i]
  possible_distances <- sapply(0:total_time,
                               get_possible_distance,
                               total_time = total_time)
  higher <- sum(possible_distances > winning_distance)
  return(higher)
}


prod(sapply(1:length(time),
            distance_vector))

new_time <- as.numeric(paste0(time,collapse = ""))
new_distance <- as.numeric(paste0(distance,collapse = ""))

charge1 <- +new_time/2 + sqrt((-new_time/2)^2)
charge2 <- +new_time/2 - sqrt((-new_time/2)^2)

charge_max <- new_time/2

dist_max <- -(charge_max^2)+charge_max*new_time

charge_diff <- dist_max - new_distance

same_dist_charge1 <- new_time/2 + sqrt((-new_time/2)^2-new_distance)
same_dist_charge2 <- new_time/2 - sqrt((-new_time/2)^2-new_distance)

same_dist_charge1 - same_dist_charge2
