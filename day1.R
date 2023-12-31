#day 1
rm(list = ls())
library(readr)
input <- readLines("day1.txt")

just_numbers <-
  sapply(input,
       function(x)gsub("[A-z]","",x))

#get first and last and make numeric
two_digits <- function(v){
  v <- unlist(strsplit(v,""))
  first <- v[1]
  last <- v[length(v)]
  
  return(as.numeric(paste0(first,last)))
}

#task 1 solution
lapply(just_numbers,
       two_digits) %>% 
  unlist() %>% 
  sum()

#task 2 solution
dictionary <- data.frame(string = c("eight",
                                    "one",
                                    "two",
                                    "three",
                                    "four",
                                    "five",
                                    "six",
                                    "seven",
                                    "nine"),
                         digit = c(8,1,2,3,4,5,6,7,9))

replace_strings <- function(input_string){
  new_numbers <- data.frame(number= numeric(),
                            position = numeric())
  for(i in 1:9){
    string <- dictionary$string[i]
    digit <- dictionary$digit[i]
    
    if(!grepl(string,input_string))next
    
    positions <- unlist(gregexpr(string,input_string))
    
    new_numbers <- rbind(new_numbers,
                         data.frame(number = digit,
                                    position = positions))
    
    }
  
  number_locations <- unlist(gregexpr("[0-9]",input_string))
  if(number_locations[1] != -1){
    normal_numbers <- unlist(strsplit(input_string,""))[number_locations]
    
    new_numbers <- rbind(new_numbers,
                         data.frame(number = normal_numbers,
                                    position = number_locations))
  }

  input_string <- paste0(new_numbers$number[order(new_numbers$position)],collapse ="")
  return(input_string)
}
#replace string numbers 
add_digits <- sapply(input,
                     replace_strings)

sum(sapply(add_digits,
           two_digits))
  