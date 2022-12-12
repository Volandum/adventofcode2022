input_lines = readLines('./inputs/12.txt')
y_dimension = length(input_lines)
x_dimension = unique(nchar(input_lines))
data_vector = unlist(strsplit(input_lines, ''))

table(data_vector)

height_vector_letters = ifelse(data_vector == 'S', 'a',
                               ifelse(data_vector == 'E', 'z',
                                      data_vector))
height_vector = match(height_vector_letters, letters)


adjacent_squares = function(onedimpos){
  x = (onedimpos - 1) %% x_dimension + 1
  y = (onedimpos - 1) %/% x_dimension + 1
  
  squares = c(
    (onedimpos - 1)[x > 1],
    (onedimpos + 1)[x < x_dimension],
    (onedimpos - x_dimension)[y > 1],
    (onedimpos + x_dimension)[y < y_dimension]
  )
  from_squares = c(
    onedimpos[x > 1],
    onedimpos[x < x_dimension],
    onedimpos[y > 1],
    onedimpos[y < y_dimension]
  )
  return(squares[height_vector[squares]<=(height_vector[from_squares] + 1)])
}

time_to_vector = rep(-1, length(height_vector))

positions_reached = which(data_vector == 'S')
newly_reached_positions = positions_reached
time_to_vector[newly_reached_positions] = 0
for(timestep in 1:length(height_vector)){
  newly_reached_positions = setdiff(adjacent_squares(newly_reached_positions), positions_reached)
  if(length(newly_reached_positions) == 0){
    break
  }
  positions_reached = c(positions_reached, newly_reached_positions)
  time_to_vector[newly_reached_positions] = timestep
}
time_to_vector[data_vector == 'E']
#456

adjacent_squares_reversed = function(onedimpos){
  x = (onedimpos - 1) %% x_dimension + 1
  y = (onedimpos - 1) %/% x_dimension + 1
  
  squares = c(
    (onedimpos - 1)[x > 1],
    (onedimpos + 1)[x < x_dimension],
    (onedimpos - x_dimension)[y > 1],
    (onedimpos + x_dimension)[y < y_dimension]
  )
  from_squares = c(
    onedimpos[x > 1],
    onedimpos[x < x_dimension],
    onedimpos[y > 1],
    onedimpos[y < y_dimension]
  )
  return(squares[height_vector[from_squares]<=(height_vector[squares] + 1)])
}
positions_reached = which(data_vector == 'E')
time_to_vector = rep(-1, length(height_vector))
newly_reached_positions = positions_reached
time_to_vector[newly_reached_positions] = 0
for(timestep in 1:length(height_vector)){
  newly_reached_positions = setdiff(adjacent_squares_reversed(newly_reached_positions), positions_reached)
  if(length(newly_reached_positions) == 0){
    break
  }
  positions_reached = c(positions_reached, newly_reached_positions)
  time_to_vector[newly_reached_positions] = timestep
}
times_from_a = time_to_vector[height_vector_letters == 'a']
min(times_from_a[times_from_a != -1])
#454