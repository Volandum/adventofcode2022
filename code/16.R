input_lines = readLines('./inputs/16.txt')
input_data = 
  data.frame(
    valve_name = split_vectorised(input_lines, '=|;|(valves? )|(Valve )|( has )', 2),
    flow_rate = as.numeric(split_vectorised(input_lines, '=|;|(valves? )|(Valve )|( has )', 4)),
    destinations = split_vectorised(input_lines, '=|;|(valves? )|(Valve )|( has )', 6)
)

valves_of_interest = input_data %>% filter(flow_rate > 0) %>% pull(valve_name) %>% union('AA')

edge_df_list = lapply(
  1:nrow(input_data),
  \(rownum){
    data.frame(node1 = input_data$valve_name[rownum], 
               node2 = el(strsplit(input_data$destinations[rownum], ', ')))}
)

edge_df = data.table::rbindlist(edge_df_list)
# Is this symmetric?
nrow(unique(rbind(edge_df, 
                  data.frame(node1 = edge_df$node2, node2 = edge_df$node1)))) # Yes
valve_graph = graph_from_data_frame(edge_df, directed = F)

distance_matrix = distances(valve_graph, v = valves_of_interest, to = valves_of_interest) + 1
valves_and_values = structure(input_data[input_data$valve_name %in% valves_of_interest, 'flow_rate'],
                              names = input_data[input_data$valve_name %in% valves_of_interest, 'valve_name'])

# Try the brute force recursion approach

get_value = memoise::memoise(function(
    current_valve, turns_left, valves_left
){
  #returns list(max_value, path)
  remaining_distances = distance_matrix[current_valve, valves_left]
  potential_valves = names(remaining_distances[remaining_distances < turns_left])
  if(length(potential_valves) == 0){
    return(0)
  }
  potential_valve_values = rep(0, length(potential_valves))
  potential_paths = rep('', length(potential_valves))
  for(next_valve_index in 1:length(potential_valves)){
    next_valve = potential_valves[next_valve_index]
    new_turns_left = turns_left - remaining_distances[next_valve]
    additional_value = valves_and_values[next_valve] * new_turns_left
    value_and_path = get_value(
      next_valve, new_turns_left, setdiff(potential_valves, next_valve)
    )
    if(is.numeric(value_and_path)){ #is 0
      # the potential_path is just the next valve and the value is just the additional_value
      potential_valve_values[next_valve_index] = additional_value
      potential_paths[next_valve_index] = next_valve
    } else {
      subsequent_value = value_and_path$max_value
      subsequent_path = value_and_path$path
      potential_valve_values[next_valve_index] = additional_value + subsequent_value
      potential_paths[next_valve_index] = paste(next_valve, subsequent_path)
    }
  }
  
  best_option_index = which.max(potential_valve_values)
  return(list(
    max_value = potential_valve_values[best_option_index],
    path = potential_paths[best_option_index]
  ))
})


get_value('AA', 30, valves_of_interest)
# $max_value
# [1] 2080
# 
# $path
# [1] "EI OA EK YP PU ZO"

# Part 2 - let's inspect this graph

plot(igraph::graph_from_adjacency_matrix(
  distance_matrix - 1, mode = 'undirected', weighted = TRUE),
  layout = layout_with_fr)

# I could run a recursion with 2 players but is this really the best plan?
# Let's tweak the recursion to return all paths so we can check for disjoint ones

get_all_paths = memoise::memoise(function(
    current_valve, turns_left, valves_left
){
  #returns list(values, paths)
  remaining_distances = distance_matrix[current_valve, valves_left]
  potential_valves = names(remaining_distances[remaining_distances < turns_left])
  if(length(potential_valves) == 0){
    return(0)
  }
  potential_valve_values = numeric(0)
  potential_paths = character(0)
  for(next_valve_index in 1:length(potential_valves)){
    next_valve = potential_valves[next_valve_index]
    new_turns_left = turns_left - remaining_distances[next_valve]
    additional_value = unname(valves_and_values[next_valve]) * new_turns_left
    value_and_path = get_all_paths(
      next_valve, new_turns_left, setdiff(potential_valves, next_valve)
    )
    if(is.numeric(value_and_path)){ #is 0
      # the potential_path is just the next valve and the value is just the additional_value
      potential_valve_values = c(potential_valve_values, additional_value)
      potential_paths = c(potential_paths, next_valve)
    } else {
      potential_valve_values = c(potential_valve_values, additional_value)
      potential_paths = c(potential_paths, next_valve)
      subsequent_values = value_and_path$values
      subsequent_paths = value_and_path$paths
      potential_valve_values = c(potential_valve_values, additional_value + subsequent_values)
      potential_paths = c(potential_paths, paste(next_valve, subsequent_paths))
    }
    # if(length(potential_valve_values) != length(potential_paths)){
    #   browser()
    # }
    
  }
  return(list(
    values = potential_valve_values,
    paths = potential_paths
  ))
})

length_26_paths = get_all_paths('AA', 26, setdiff(valves_of_interest, 'AA'))
length_26_paths_df = data.frame(
  value = length_26_paths$values,
  path = length_26_paths$paths
)

# Now we just need to join this 60K row table to itself to find disjoint paths - unfortunately I don't have 300GB of memory
for(valve in setdiff(valves_of_interest, 'AA')){
  length_26_paths_df[[valve]] = grepl(valve, length_26_paths_df$path)
}

second_df = data.frame(
  other_value = length_26_paths$values,
  other_path = length_26_paths$paths
)

for(valve in setdiff(valves_of_interest, 'AA')){
  second_df[[valve]] = grepl(valve, second_df$other_path)
}

# paste0('select * from length_26_paths_df inner join second_df on ',
#        paste0('not (length_26_paths_df.',
#               setdiff(valves_of_interest, 'AA'),
#               ' and second_df.',
#               setdiff(valves_of_interest, 'AA'),
#               ')', collapse = ' and '))
# 
# combinations = sqldf(paste0('select * from length_26_paths_df inner join second_df on ',
#              paste0('not (length_26_paths_df.',
#                     setdiff(valves_of_interest, 'AA'),
#                     ' and second_df.',
#                     setdiff(valves_of_interest, 'AA'),
#                     ')', collapse = ' and '))
# ) #Out of memory - let's do a hundred at a time

process_a_chunk = function(smaller_table){
  temp_smaller_table = smaller_table
  combinations = sqldf(paste0('select value, path, other_value, other_path from temp_smaller_table inner join second_df on ',
                              paste0('not (temp_smaller_table.',
                                     setdiff(valves_of_interest, 'AA'),
                                     ' and second_df.',
                                     setdiff(valves_of_interest, 'AA'),
                                     ')', collapse = ' and ')))
  return(combinations %>% mutate(total_value = value + other_value) %>% slice_max(total_value, n = 1))
}

blocks = split(length_26_paths_df, 1:nrow(length_26_paths_df) %/% 1000)
processed_blocks = lapply(blocks, process_a_chunk) # Terribly slow here
data.table::rbindlist(processed_blocks) %>% arrange(total_value)
# 2752