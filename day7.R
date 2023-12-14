#day 7
rm(list = ls())
library(tidyverse)

input <- read.table("day7.txt")
input2 <- input
test <- read.table("test_day7.txt")
names(input) <- c("hand","bid")
names(test) <- names(input)
translate_cards <- function(hand,
                            task2 = FALSE){
  hand_sep <- unlist(strsplit(hand,""))
  cards <- c("A","K","Q","J","T",9:2)
  if(task2){
    cards <- c("A","K","Q","T",9:2,"J")
  }
  values <- c(14:2)
  translated_hand <- values[match(hand_sep,cards)]
  return(translated_hand)
}

get_strength <- function(translated_hand,
                         task2 = FALSE){
  table <- table(translated_hand)
  if(task2){
    if(2 %in% translated_hand & length(table) > 1){
      no_jokers <- table(translated_hand[translated_hand != 2])
      to_replace <- names(no_jokers[no_jokers == max(no_jokers)])
      if(length(to_replace) > 1){
        to_replace <- to_replace[1]
      }
      translated_hand[translated_hand == as.numeric(to_replace)] <- 2
      table <- table(translated_hand)
    }
  }
  #five of a kind
  if(length(table) == 1){
    return(7)
  }
  #four of a kind
  if(max(table) == 4){
      return(6)
  }
  #full house
  if(length(table)==2){
      return(5)
  }
  #three of a kind
  if(max(table) == 3){
      return(4)
  }
  #two pair
  if(length(table) == 3){
      return(3)
  }
  #one pair
  if(length(table)==4){
      return(2)
  }
  #highcard
  if(length(table) == 5){
      return(1)
  }
}

process_hand <- function(input_row,
                         input,
                         task2 = FALSE){
  input_row <- input[input_row,]
  hand <- input_row$hand
  translated <- translate_cards(hand,
                                task2 = task2)
  strength <- get_strength(translated,
                           task2 = task2)
  new_info <- as.data.frame(cbind(strength,t(translated)))
  names(new_info) <- c("strength",paste0("card",1:5))
  boosted_input_row <- cbind(input_row, new_info)
  return(boosted_input_row)
}

order_hands_by_strength <- function(input,
                                    task2 = FALSE){
  #look at each hand and add its strenght to table
  boosted_input <- lapply(1:nrow(input),
                          process_hand,
                          input,
                          task2) %>% 
    bind_rows()
  
  boosted_input %>% 
    arrange(strength,card1,card2,
            card3,card4,card5) %>% 
    rowid_to_column("rank") %>% 
    mutate(result = rank*bid) %>% 
    return()

}
#task 1 
test_result <- order_hands_by_strength(input, task2 = FALSE)
sum(test_result$result)

#task 2
test_result <- order_hands_by_strength(input, task2 = TRUE)
sum(test_result$result)

