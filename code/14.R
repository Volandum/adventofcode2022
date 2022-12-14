input_lines = readLines('./inputs/14.txt')
all_coordinates = unlist(strsplit(input_lines, ' -> ', fixed = T))
summary(as.numeric(split_vectorised(all_coordinates, ',', 1)))
summary(as.numeric(split_vectorised(all_coordinates, ',', 2)))
#462-534, 13-171 means that naive approaches will work

x_offset = min(as.numeric(split_vectorised(all_coordinates, ',', 1))) - 2
x_width = max(as.numeric(split_vectorised(all_coordinates, ',', 1))) + 1 - x_offset
height = max(as.numeric(split_vectorised(all_coordinates, ',', 2)))

cave_matrix = matrix(0, nrow = x_width, ncol = height + 1)

for(line in input_lines){
  coordinate_sequence = unlist(strsplit(line, ' -> ', fixed = T))
  number_of_coordinates = length(coordinate_sequence)
  x_coordinates = as.numeric(split_vectorised(coordinate_sequence, ',', 1))
  y_coordinates = as.numeric(split_vectorised(coordinate_sequence, ',', 2))
  for(coordinate_number in 1:(length(coordinate_sequence) - 1)){
    if(x_coordinates[coordinate_number] == x_coordinates[coordinate_number + 1]){
      cave_matrix[x_coordinates[coordinate_number] - x_offset, 
             y_coordinates[coordinate_number]:y_coordinates[coordinate_number + 1]] = 2
    } else {
      cave_matrix[(x_coordinates[coordinate_number]:x_coordinates[coordinate_number + 1])-x_offset,
             y_coordinates[coordinate_number]] = 2
    }
  }
}

sand_stream_x = c(500) - x_offset
sand_stream_y = c(0)
sand_stream_length = 1
while(sand_position_y <= height){
  sand_position_x = sand_stream_x[sand_stream_length]
  sand_position_y = sand_stream_y[sand_stream_length]
  if(cave_matrix[sand_position_x, sand_position_y + 1] == 0){
    sand_position_y = sand_position_y + 1
    sand_stream_x = c(sand_stream_x, sand_position_x)
    sand_stream_y = c(sand_stream_y, sand_position_y)
    sand_stream_length = sand_stream_length + 1
  } else if(cave_matrix[sand_position_x - 1, sand_position_y + 1] == 0){
    sand_position_x = sand_position_x - 1
    sand_position_y = sand_position_y + 1
    sand_stream_x = c(sand_stream_x, sand_position_x)
    sand_stream_y = c(sand_stream_y, sand_position_y)
    sand_stream_length = sand_stream_length + 1
  } else if(cave_matrix[sand_position_x + 1, sand_position_y + 1] == 0){
    sand_position_x = sand_position_x + 1
    sand_position_y = sand_position_y + 1
    sand_stream_x = c(sand_stream_x, sand_position_x)
    sand_stream_y = c(sand_stream_y, sand_position_y)
    sand_stream_length = sand_stream_length + 1
  } else {
    cave_matrix[sand_position_x, sand_position_y] = 1
    sand_stream_x = sand_stream_x[1:(sand_stream_length - 1)]
    sand_stream_y = sand_stream_y[1:(sand_stream_length - 1)]
    sand_stream_length = sand_stream_length - 1
  }
}

table(cave_matrix)
#1133

#part 2 needs much more space
x_offset = x_offset - height
x_width = x_width + height * 2
cave_matrix = matrix(0, nrow = x_width, ncol = height + 2)

for(line in input_lines){
  coordinate_sequence = unlist(strsplit(line, ' -> ', fixed = T))
  number_of_coordinates = length(coordinate_sequence)
  x_coordinates = as.numeric(split_vectorised(coordinate_sequence, ',', 1))
  y_coordinates = as.numeric(split_vectorised(coordinate_sequence, ',', 2))
  for(coordinate_number in 1:(length(coordinate_sequence) - 1)){
    if(x_coordinates[coordinate_number] == x_coordinates[coordinate_number + 1]){
      cave_matrix[x_coordinates[coordinate_number] - x_offset, 
                  y_coordinates[coordinate_number]:y_coordinates[coordinate_number + 1]] = 2
    } else {
      cave_matrix[(x_coordinates[coordinate_number]:x_coordinates[coordinate_number + 1])-x_offset,
                  y_coordinates[coordinate_number]] = 2
    }
  }
}

cave_matrix[,173] = 3

sand_stream_x = c(500) - x_offset
sand_stream_y = c(0)
sand_stream_length = 1
while(sand_stream_length > 0){
  sand_position_x = sand_stream_x[sand_stream_length]
  sand_position_y = sand_stream_y[sand_stream_length]
  if(cave_matrix[sand_position_x, sand_position_y + 1] == 0){
    sand_position_y = sand_position_y + 1
    sand_stream_x = c(sand_stream_x, sand_position_x)
    sand_stream_y = c(sand_stream_y, sand_position_y)
    sand_stream_length = sand_stream_length + 1
  } else if(cave_matrix[sand_position_x - 1, sand_position_y + 1] == 0){
    sand_position_x = sand_position_x - 1
    sand_position_y = sand_position_y + 1
    sand_stream_x = c(sand_stream_x, sand_position_x)
    sand_stream_y = c(sand_stream_y, sand_position_y)
    sand_stream_length = sand_stream_length + 1
  } else if(cave_matrix[sand_position_x + 1, sand_position_y + 1] == 0){
    sand_position_x = sand_position_x + 1
    sand_position_y = sand_position_y + 1
    sand_stream_x = c(sand_stream_x, sand_position_x)
    sand_stream_y = c(sand_stream_y, sand_position_y)
    sand_stream_length = sand_stream_length + 1
  } else {
    cave_matrix[sand_position_x, sand_position_y] = 1
    sand_stream_x = sand_stream_x[1:(sand_stream_length - 1)]
    sand_stream_y = sand_stream_y[1:(sand_stream_length - 1)]
    sand_stream_length = sand_stream_length - 1
  }
}
table(cave_matrix)
#27565, add 1 for the (500,0) point