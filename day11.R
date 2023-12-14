#day 11
rm(list = ls())
library(tidyverse)

input <- readLines("day11.txt")

parse_line <- function(line){
  strsplit(line,"") %>% 
    unlist() %>% 
    return()
}
universe <-lapply(input,
                  parse_line)

universe <- do.call(rbind,universe)

#expand the universe 
empty_rows <- apply(universe,
                    1,
                    function(x) "#" %in% x)
empty_rows <- which(empty_rows == FALSE)

empty_cols <- apply(universe,
                    2,
                    function(x) "#" %in% x)
empty_cols <- which(empty_cols == FALSE)

expanded_universe <- universe
add <- 0
for(row in empty_rows){
  row <- row + add
  expanded_universe <- rbind(expanded_universe[1:row,],
                expanded_universe[row:nrow(expanded_universe),])
  add <- add +1
}
add <- 0
for(col in empty_cols){
  col <- col + add
  expanded_universe <- cbind(expanded_universe[,1:col],
                             expanded_universe[,col:ncol(expanded_universe)])
  add <- add +1
}

galaxies <- which(expanded_universe == "#",
                  arr.ind = TRUE)

distances <- dist(galaxies, method = "manhattan")

distances <- as.matrix(distances)
sum(distances[upper.tri(distances)])

#### PART 2####
galaxies <- which(universe == "#",
                  arr.ind = TRUE)
nrow(galaxies)

galaxy_pairs <- as.data.frame(t(combn(1:nrow(galaxies),2)))
galaxy_pairs$distance <- NA
galaxy_pairs$mill <- NA

for(pair in 1:nrow(galaxy_pairs)){
  #get actual distance 
  #two points
  A <- galaxies[galaxy_pairs$V1[pair],]
  B <- galaxies[galaxy_pairs$V2[pair],]
  
  distance <- dist(rbind(A,B), method = "manhattan")
  
  extra_cols <- sum(empty_cols %in% A[2]:B[2])
  extra_rows <- sum(empty_rows %in% A[1]:B[1])
  
  added_distance <- extra_cols * 999999 + extra_rows*999999
  
  distance <- distance + added_distance
  
  galaxy_pairs$distance[pair] <- distance
}
head(galaxy_pairs)
sum(galaxy_pairs$distance)
