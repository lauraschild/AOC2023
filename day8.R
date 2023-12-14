#day8
rm(list = ls())
library(tidyverse)

input <- readLines("day8.txt")

instructions <- input[1] %>% 
  strsplit("") %>% 
  unlist()

network <- input[-c(1:2)] %>% 
  lapply(function(x) unlist(strsplit(x,""))) %>% 
  lapply(function(x) x[grepl("[A-Z]",x)]) %>%
  lapply(function(x)data.frame(node = paste0(x[1:3],collapse = ""),
                               left = paste0(x[4:6],collapse = ""),
                               right = paste0(x[7:9],collapse = ""))) %>% 
  bind_rows()

final_destination <- FALSE
current_instruction <- 0
current_node <- "AAA"
steps <- 0
while(!final_destination){
  #go to next instruction
  current_instruction <- current_instruction +1
  #start from the beginning if we went through
  if(current_instruction == (length(instructions)+1)){
    current_instruction <- 1
  }
  #get the direction
  current_direction <- instructions[current_instruction]
  #translate the direction
  current_direction <- ifelse(current_direction == "L",
                              "left",
                              "right")
  #get new current node
  current_node <- network %>% 
    filter(node == current_node) %>% 
    pull(get(current_direction))
  
  #add one to the steps needed
  steps <- steps +1
  
  #destination_reached?
  final_destination <- ifelse(current_node == "ZZZ",
                              TRUE,
                              FALSE)
}

steps

####TASK 2####
get_starting_nodes <- function(network){
  network$node %>% 
    lapply(function(x) data.frame(node = x,t(unlist(strsplit(x,""))))) %>% 
    bind_rows() %>% 
    filter(X3 == "A") %>% 
    pull(node) %>% 
    return()
}

current_nodes <- get_starting_nodes(network)
Z_frame <- data.frame(node = character(),
                      Z_node = character(),
                      instruction = numeric(),
                      steps = numeric())
#find periodicity
for(node in current_nodes){
  #find periodicity in getting to a Z node
  #record each Z node found, the current instruction and the number of steps it took
  #stop when the same Z node was found at the same current instruction again
  current_node <- node
  starting_node <- node
  period <- FALSE
  current_instruction <- 0
  steps <- 0  
  while(!period){
    
    #go to next instruction
    current_instruction <- current_instruction +1
    #start from the beginning if we went through
    if(current_instruction == (length(instructions)+1)){
      current_instruction <- 1
    }
    #get the direction
    current_direction <- instructions[current_instruction]
    #translate the direction
    current_direction <- ifelse(current_direction == "L",
                                "left",
                                "right")
    
    #get new current node
    current_node <- network %>% 
      filter(node == current_node) %>% 
      pull(get(current_direction))
    
    #add one to the steps needed
    steps <- steps +1
    
    #check if current_node ends with Z
    Z_node <- unlist(strsplit(current_node,""))[3] == "Z"
    
    #add to Z data frame if yes
    if(Z_node){
      Z_frame <- rbind(Z_frame,
                       data.frame(node = starting_node,
                                  Z_node = current_node,
                                  instruction = current_instruction,
                                  steps = steps))
      
      #check for periodicity
      period <- Z_frame %>% 
        filter(node == starting_node) %>% 
        group_by(node,Z_node,instruction) %>% 
        mutate(count = n()) %>% 
        pull(count) %>% 
        max()==2
    }
    
  }
}
steps <- Z_frame %>% 
  group_by(node) %>% 
  filter(steps == min(steps)) %>% 
  pull()

#least common multiple
#print without scientific notation
sprintf("%.0f",numbers::mLCM(steps))
