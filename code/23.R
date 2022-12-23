input_lines = readLines('./inputs/23.txt')
horizontal_width = min(nchar(input_lines))
vertical_height = length(input_lines)
map_matrix = matrix(unlist(strsplit(input_lines, '')),
                    nrow = vertical_height, byrow = T)

elf_locations_vectors = which(map_matrix == '#', arr.ind = TRUE)

elf_locations_df = 
  data.frame(elfid = 1:(dim(elf_locations_vectors)[1]),
             x = elf_locations_vectors[,2],
             y = -elf_locations_vectors[,1])
elf_locations_df$complex_location = elf_locations_df$x + 1i*elf_locations_df$y
elf_locations_short = elf_locations_df[,c('elfid','complex_location')]

get_proposals = function(elf_position_dataframe, turn_order){
  current_locations = elf_position_dataframe$complex_location
  directions = c(E=1, NE=1+1i, N=1i, NW=-1+1i,W=-1,SW=-1-1i, S=-1i, SE=1-1i)
  has_neighbour = lapply(directions,
                         function(direction){
                           elf_position_dataframe[(elf_position_dataframe$complex_location + direction) %in% current_locations,
                                            'elfid']})
  has_any_neighbour = Reduce(union, has_neighbour)
  proposing_elves = has_any_neighbour
  proposed_directions = data.frame()
  for(direction_scan_index in 0:3){
    check_direction = (turn_order + direction_scan_index) %% 4 # 1 = N, 2 = S, 3 = W, 0 = E
    if(check_direction == 1){
      invalid_elves = 
        union(has_neighbour$NE, union(has_neighbour$N, has_neighbour$NW))
      valid_elves_for_direction = setdiff(proposing_elves, invalid_elves)
      proposed_directions = 
        elf_position_dataframe %>% filter(elfid %in% valid_elves_for_direction) %>%
        mutate(proposed_location = complex_location + 1i) %>% 
        rbind(proposed_directions)
      proposing_elves = setdiff(proposing_elves, valid_elves_for_direction)
    } else if (check_direction == 2){
      invalid_elves = 
        union(has_neighbour$SE, union(has_neighbour$S, has_neighbour$SW))
      valid_elves_for_direction = setdiff(proposing_elves, invalid_elves)
      proposed_directions = 
        elf_position_dataframe %>% filter(elfid %in% valid_elves_for_direction) %>%
        mutate(proposed_location = complex_location - 1i) %>% 
        rbind(proposed_directions)
      proposing_elves = setdiff(proposing_elves, valid_elves_for_direction)
    } else if (check_direction == 3){
      invalid_elves = 
        union(has_neighbour$SW, union(has_neighbour$W, has_neighbour$NW))
      valid_elves_for_direction = setdiff(proposing_elves, invalid_elves)
      proposed_directions = 
        elf_position_dataframe %>% filter(elfid %in% valid_elves_for_direction) %>%
        mutate(proposed_location = complex_location - 1) %>% 
        rbind(proposed_directions)
      proposing_elves = setdiff(proposing_elves, valid_elves_for_direction)
    } else if (check_direction == 0){
      invalid_elves = 
        union(has_neighbour$SE, union(has_neighbour$E, has_neighbour$NE))
      valid_elves_for_direction = setdiff(proposing_elves, invalid_elves)
      proposed_directions = 
        elf_position_dataframe %>% filter(elfid %in% valid_elves_for_direction) %>%
        mutate(proposed_location = complex_location + 1) %>% 
        rbind(proposed_directions)
      proposing_elves = setdiff(proposing_elves, valid_elves_for_direction)
    } else {
      stop('unknown direction')
    }
  }
  return(proposed_directions)
}

make_moves = function(elf_position_dataframe, direction_proposals){
  valid_proposals = direction_proposals %>%
    group_by(proposed_location) %>% filter(n() == 1) %>% ungroup()
  rbind(elf_position_dataframe %>% 
          filter(!(elfid %in% valid_proposals$elfid)),
  valid_proposals %>% 
    transmute(elfid, complex_location = proposed_location))
}

simulation = list()
working_locations = elf_locations_short
simulation$`0` = working_locations
for(round in 1:10){
  working_locations = 
    make_moves(working_locations,
               get_proposals(working_locations, round))
  simulation[[as.character(round)]] = working_locations
}

new_locations = simulation$`10`$complex_location
vertical_extent = max(Im(new_locations)) - min(Im(new_locations)) + 1
horizontal_extent = max(Re(new_locations)) - min(Re(new_locations)) + 1
vertical_extent * horizontal_extent - length(new_locations)
#4138

make_moves = function(elf_position_dataframe, direction_proposals){
  valid_proposals = direction_proposals %>%
    group_by(proposed_location) %>% filter(n() == 1) %>% ungroup()
  if(nrow(valid_proposals) == 0){
    return(NA)
  }
  message(nrow(valid_proposals))
  rbind(elf_position_dataframe %>% 
          filter(!(elfid %in% valid_proposals$elfid)),
        valid_proposals %>% 
          transmute(elfid, complex_location = proposed_location))
}


simulation = list()
working_locations = elf_locations_short
simulation$`0` = working_locations
for(round in 1:5000){
  if(round %% 50 == 0){
    message(round)
  }
  working_locations = 
    make_moves(working_locations,
               get_proposals(working_locations, round))
  if(!is.data.frame(working_locations)){
    break
  }
  simulation[[as.character(round)]] = working_locations
}
round
#1010