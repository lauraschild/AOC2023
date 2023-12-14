#day 5


  rm(list = ls())
  library(tidyverse)
  
  input <- readLines("day5.txt")
  #get the seeds
  seeds <- unlist(strsplit(input[1]," "))[-1]
  
  #make the maps
  mapping_boundaries <- which(input == "")
  mapping_boundaries <-data.frame(start = mapping_boundaries +1,
                                  end = c(mapping_boundaries[-1]-1,length(input)))
  
  names <- c()
  
  get_map <- function(row){
    #get the individual map input
    start <- mapping_boundaries$start[row]
    end <- mapping_boundaries$end[row]
    map_input <- input[start:end]
    
    #get source and destination names
    names <<- c(names,unlist(strsplit(gsub(" map:","",map_input[1]),"-to-"))[2])
    
    #split starts and ranges
    numbers <-lapply(map_input[-1],
                     function(x) as.numeric(unlist(strsplit(x," "))))
    numbers <-do.call(rbind,numbers) %>% 
      as.data.frame()
    
    names(numbers) <- c("dest_start","source_start","range")
    
    return(numbers)
  }
  
  map <- lapply(1:nrow(mapping_boundaries),
                get_map)
  names(map) <- names
  
  #function to get translated number from a map
  convert <- function(sub_map,
                      source_number){
    #go through all source starts and check if source number is in range
    for(starts in 1:nrow(sub_map)){
      start <- sub_map$source_start[starts]
      end <- sub_map$source_start[starts] + sub_map$range[starts]
      in_range <- source_number >=  start & source_number<= end
      
      if(in_range){
        correct_row <- starts
        break
      }
      if(!in_range & starts == nrow(sub_map)){
        #if none of the ranges match, the dest is equal to source
        destination_number <- source_number
        return(destination_number)
      }
    }
    
    #translate source to correct dest
    start <- sub_map$source_start[correct_row]
    need_to_add <- source_number - start
    destination_number <- sub_map$dest_start[correct_row] + need_to_add
    return(destination_number)
  }
  
  #wrapper function to take a seed number and translate completely
  from_seed_to_loc <- function(seed_number){
    source_number <- seed_number
    for(conversion in 1:length(map)){
      sub_map <- map[[conversion]]
      destination_number <- convert(sub_map,
                                    source_number)
      source_number <- destination_number
      if(conversion == length(map)){
        location <- destination_number
      }
    }
    return(location)
  }
  
  
  min(sapply(as.numeric(seeds),from_seed_to_loc))
  
  seeds2 <- lapply(seq(1,length(seeds),2),
                   function(x)data.frame(start = as.numeric(seeds[x]), range = (as.numeric(seeds[x+1])))) %>% 
    bind_rows()
  
  convert_to_ranges <- function(sub_map,
                               source_ranges){
    
    destination_ranges <- data.frame(start = numeric(),
                                     range = numeric())
    
    for(range_row in 1:nrow(source_ranges)){
      source_start <- source_ranges$start[range_row]
      source_range <- source_ranges$range[range_row]
      source_end <- source_start + source_range
      
  
      found_overlap <- data.frame(start = numeric(),
                                  range = numeric())
      for(map_row in 1:nrow(sub_map)){
        map_start <- sub_map$source_start[map_row]
        map_range <- sub_map$range[map_row]
        map_end <- map_start + map_range
        
        #is there any overlap
        overlap <- !(source_end < map_start|source_start > map_end)
        #next row if not
        if(!overlap){
          next
        }
        
        delta_start <- map_start - source_start
        delta_end <- map_end - source_end
        
        if(delta_start>=0 & delta_end >=0){
          new_range <- source_range - delta_start
          new_start <- map_start
        }
        if(delta_start <= 0 & delta_end >= 0){
          new_range <- source_range
          new_start <- source_start
        }
        if(delta_start >=0 & delta_end <= 0){
          new_range <- map_range
          new_start <- map_start
        }
        if(delta_start <= 0 & delta_end <=0){
          new_range <- source_range + delta_end
          new_start <- source_start
        }
        
        found_overlap <- rbind(found_overlap,
                               data.frame(start = new_start,
                                          range = new_range))
        dest_start <- sub_map$dest_start[map_row]
        conversion_factor <- dest_start - map_start
        
        translated_start <- new_start + conversion_factor
        
        destination_ranges <- rbind(destination_ranges,
                                    data.frame(start = translated_start,
                                               range= new_range))
      }
      
      #check if any parts of the range were not matched
      
      all_there <- min(found_overlap$start) + sum(found_overlap$range) +length(found_overlap)-1 == source_end
      
      if(!all_there){
        found_overlap$end <- found_overlap$start + found_overlap$range
        found_overlap <-found_overlap %>%
          arrange(start)
        
        missing_ranges <- !c((source_start),found_overlap$end) == c(found_overlap$start,source_end)
        
        missing_starts <- c(source_start,(found_overlap$end))[missing_ranges]
        missing_ranges <- (c(found_overlap$start,source_end)-(c(source_start,(found_overlap$end))))[missing_ranges]
        
        destination_ranges <- rbind(destination_ranges,
                                    data.frame(start = missing_starts,
                                               range = missing_ranges))
      }
    }
    
    return(destination_ranges)
  }
  
  
  #wrapper function to take a seed number and translate completely
  from_seed_to_loc <- function(seed_ranges){
    source_ranges <- seed_ranges
    for(conversion in 1:length(map)){
      sub_map <- map[[conversion]]
      destination_ranges <- convert_to_ranges(sub_map,
                                              source_ranges)
      source_ranges <- destination_ranges
      if(conversion == length(map)){
        locations <- destination_ranges$start
      }
    }
    return(locations)
  }
  
  locations <- from_seed_to_loc(seeds2)
  min(locations)

  
