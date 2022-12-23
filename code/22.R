input_lines = readLines('./inputs/22.txt')
directions_string = input_lines[length(input_lines)]
#directions_string = '10L7R4R9R12R83R23R'
directions_vector = unlist(regmatches(directions_string, gregexec('\\d+\\w', directions_string)))
directions_count = length(directions_vector)
directions = data.frame(
  distance = as.numeric(gsub('[LR]', '', directions_vector)),
  turn = gsub('\\d+', '', directions_vector)
)
# Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).

facing_vector = c(0, cumsum(ifelse(directions$turn == 'L', -1, ifelse(directions$turn == 'R', 1, 0))) %% 4)

path = data.frame(
  id = 1:nrow(directions),
  directions_distance = directions$distance,
  directions_turn = directions$turn,
  starting_horizontal_coord = -1,
  starting_vertical_coord = -1,
  starting_facing = facing_vector[1:directions_count],
  ending_horizontal_coord = -1,
  ending_vertical_coord = -1,
  ending_facing = facing_vector[2:(directions_count + 1)],
  distance_moved = -1
)

map_string = input_lines[1:(length(input_lines) - 2)]
horizontal_width = max(nchar(map_string))
pad_string = Vectorize(function(string_line){
  paste0(string_line,
         paste0(rep(' ', horizontal_width - nchar(string_line)),
                collapse = ''))
})
padded_string = pad_string(map_string)
characters_1d = strsplit(padded_string, '') %>%
  unlist %>%
  unname 
map_matrix = matrix(ifelse(characters_1d == ' ', -1, ifelse(characters_1d == '#', 1, 0)),
       nrow = length(map_string), byrow = T)
starting_point_vertical = 1
starting_point_horizontal = min(which(map_matrix[1,] == 0))

characterise_move = function(direction, starting_horizontal, starting_vertical){
  # Returns a function giving the ending coordinate given the distance moved
  if(direction %in% c(0, 2)){
    orientation = 'horizontal'
    line = map_matrix[starting_vertical,]
    starting_position = starting_horizontal
  } else {
    orientation = 'vertical'
    line = map_matrix[, starting_horizontal]
    starting_position = starting_vertical
  }
  direction_polarity = ifelse(direction %in% c(0, 1), 1, -1)
  left_edge = min(which(line != -1))
  right_edge = max(which(line != -1))
  width = right_edge - left_edge + 1
  subline = line[left_edge:right_edge]
  if(any(subline < 0)){
    browser()
    stop('intervening non-pathable points')
  }
  relative_starting_position = starting_position - left_edge # in [0, width)
  obstacle_positions = which(subline == 1) - 1 #in [0, width) as well
  if(length(obstacle_positions) == 0){
    # no obstacles
    if(orientation == 'horizontal'){
      return_fun = function(distance){
        horizontal_coord = 
          left_edge + (relative_starting_position + direction_polarity * distance) %% width
        return(list(horizontal_coord = horizontal_coord,
                    vertical_coord = starting_vertical,
                    distance_moved = distance))
      }
    } else if(orientation == 'vertical'){
      return_fun = function(distance){
        vertical_coord = 
          left_edge + (relative_starting_position + direction_polarity * distance) %% width
        return(list(horizontal_coord = starting_horizontal,
                    vertical_coord = vertical_coord,
                    distance_moved = distance))
      }
    }
    return(return_fun)
  }
  
  # find the next obstacle and space to the next obstacle
  if(direction_polarity == 1){
    adjusted_obstacle_positions = ifelse(obstacle_positions < relative_starting_position,
                                         obstacle_positions + width, obstacle_positions)
    next_adjusted_obstacle_position = min(adjusted_obstacle_positions)
    max_move = next_adjusted_obstacle_position - relative_starting_position - 1
  } else {
    adjusted_obstacle_positions = ifelse(obstacle_positions > relative_starting_position,
                                         obstacle_positions - width, obstacle_positions)
    next_adjusted_obstacle_position = max(adjusted_obstacle_positions)
    max_move = relative_starting_position - next_adjusted_obstacle_position - 1
  }
  if(max_move < 0){
    browser()
    stop('Error in logic')
  }
  if(orientation == 'horizontal'){
    return_fun = function(distance){
      actual_distance = min(distance,max_move)
      horizontal_coord = 
        left_edge + (relative_starting_position + direction_polarity * actual_distance) %% width
      return(list(horizontal_coord = horizontal_coord,
                  vertical_coord = starting_vertical,
                  distance_moved = actual_distance))
    }
  } else if(orientation == 'vertical'){
    return_fun = function(distance){
      actual_distance = min(distance,max_move)
      vertical_coord = 
        left_edge + (relative_starting_position + direction_polarity * actual_distance) %% width
      return(list(horizontal_coord = starting_horizontal,
                  vertical_coord = vertical_coord,
                  distance_moved = actual_distance))
    }
  }
  return(return_fun)
}

for(instruction_index in 1:nrow(path)){
  if(instruction_index == 1){
    horizontal_position = starting_point_horizontal
    vertical_position = starting_point_vertical
  }
  path$starting_horizontal_coord[instruction_index] = horizontal_position
  path$starting_vertical_coord[instruction_index] = vertical_position
  move_function = characterise_move(path$starting_facing[instruction_index],
                                    horizontal_position, vertical_position)
  new_coords = move_function(path$directions_distance[instruction_index])
  horizontal_position = new_coords$horizontal_coord
  vertical_position = new_coords$vertical_coord
  path$ending_horizontal_coord[instruction_index] = horizontal_position
  path$ending_vertical_coord[instruction_index] = vertical_position
  path$distance_moved[instruction_index] = new_coords$distance_moved
}
with(path[nrow(path),],
     1000 * ending_vertical_coord + 4 * ending_horizontal_coord + ending_facing)
#136054 is not correct

sum(directions$distance)
# 51967 is small enough that we can do this step by step and see what happens

single_step_path = directions
horizontal_width = dim(map_matrix)[2]
vertical_height = dim(map_matrix)[1]
single_step = function(horizontal_position, vertical_position, direction){
  new_horizontal_position = horizontal_position
  new_vertical_position = vertical_position
  if(direction == 0){
    if(horizontal_position == horizontal_width || map_matrix[vertical_position, horizontal_position + 1] == -1) {
      blanks = which(map_matrix[vertical_position,] == -1)
      blanks = blanks[blanks < horizontal_position]
      if(length(blanks) == 0){
        wrap_position = 1
      } else {
        wrap_position = max(blanks) + 1
      }
      if(map_matrix[vertical_position, wrap_position] == 0){
        new_horizontal_position = wrap_position
      } else if(map_matrix[vertical_position, wrap_position] == 1){
        # go nowhere
      } else {
        stop('error r1')
      }
    } else if(map_matrix[vertical_position, horizontal_position + 1] == 0){
      new_horizontal_position = horizontal_position + 1
    } else if(map_matrix[vertical_position, horizontal_position + 1] == 1) {
      # go nowhere
    } else {
      stop('error r2')
    }
  } else if (direction == 2){
    if(horizontal_position == 1 || map_matrix[vertical_position, horizontal_position - 1] == -1) {
      blanks = which(map_matrix[vertical_position,] == -1)
      blanks = blanks[blanks > horizontal_position]
      if(length(blanks) == 0){
        wrap_position = horizontal_width
      } else {
        wrap_position = min(blanks) - 1
      }
      if(map_matrix[vertical_position, wrap_position] == 0){
        new_horizontal_position = wrap_position
      } else if(map_matrix[vertical_position, wrap_position] == 1){
        # go nowhere
      } else {
        stop('error l1')
      }
    } else if(map_matrix[vertical_position, horizontal_position - 1] == 0){
      new_horizontal_position = horizontal_position - 1
    } else if(map_matrix[vertical_position, horizontal_position - 1] == 1) {
      # go nowhere
    } else {
      stop('error l2')
    }
  } else if (direction == 1){
    if(vertical_position == vertical_height || map_matrix[vertical_position + 1, horizontal_position] == -1) {
      blanks = which(map_matrix[,horizontal_position] == -1)
      blanks = blanks[blanks < vertical_position]
      if(length(blanks) == 0){
        wrap_position = 1
      } else {
        wrap_position = max(blanks) + 1
      }
      if(map_matrix[wrap_position, horizontal_position] == 0){
        new_vertical_position = wrap_position
      } else if(map_matrix[wrap_position, horizontal_position] == 1){
        # go nowhere
      } else {
        stop('error d1')
      }
    } else if(map_matrix[vertical_position + 1, horizontal_position] == 0){
      new_vertical_position = vertical_position + 1
    } else if(map_matrix[vertical_position + 1, horizontal_position] == 1) {
      # go nowhere
    } else {
      stop('error d2')
    }
  } else if (direction == 3){
    if(vertical_position == 1 || map_matrix[vertical_position - 1, horizontal_position] == -1) {
      blanks = which(map_matrix[,horizontal_position] == -1)
      blanks = blanks[blanks > vertical_position]
      if(length(blanks) == 0){
        wrap_position = vertical_height
      } else {
        wrap_position = min(blanks) - 1
      }
      if(map_matrix[wrap_position, horizontal_position] == 0){
        new_vertical_position = wrap_position
      } else if(map_matrix[wrap_position, horizontal_position] == 1){
        # go nowhere
      } else {
        stop('error u1')
      }
    } else if(map_matrix[vertical_position - 1, horizontal_position] == 0){
      new_vertical_position = vertical_position - 1
    } else if(map_matrix[vertical_position - 1, horizontal_position] == 1) {
      # go nowhere
    } else {
      stop('error u2')
    }
  } else {
    stop('error 3')
  }
  return(list(
    new_horizontal_position = new_horizontal_position,
    new_vertical_position = new_vertical_position
  ))
}

end_of_step_horizontal_position = integer()
end_of_step_vertical_position = integer()
end_of_step_facing = integer()
for(instruction_index in 1:nrow(single_step_path)){
  if(instruction_index == 1){
    horizontal_position = starting_point_horizontal
    vertical_position = starting_point_vertical
    facing = 0
  }
  number = single_step_path$distance[instruction_index]
  for(i in 1:number){
    next_position = single_step(horizontal_position, vertical_position, facing)
    horizontal_position = next_position$new_horizontal_position
    vertical_position = next_position$new_vertical_position
  }
  if(single_step_path$turn[instruction_index] == 'R'){
    facing = (facing + 1) %% 4
  } else if(single_step_path$turn[instruction_index] == 'L'){
    facing = (facing - 1) %% 4
  }
  end_of_step_horizontal_position = c(end_of_step_horizontal_position, horizontal_position)
  end_of_step_vertical_position = c(end_of_step_vertical_position, vertical_position)
  end_of_step_facing = c(end_of_step_facing, facing)
  
}
single_step_path$id = 1:nrow(single_step_path)
single_step_path$ss_end_horizontal = end_of_step_horizontal_position
single_step_path$ss_end_vertical = end_of_step_vertical_position
single_step_path$ss_end_facing = end_of_step_facing

compare = merge(path, single_step_path)
which(compare$ss_end_facing != compare$ending_facing)
compare[2000:2001,]


# Part 2:
# For the matrix generate a matrix of target x- and y- coordinates for going in each direction
visualisation = matrix(rep(0, 12), nrow = 4, byrow = T)
for(vert in 1:4){
  for(horiz in 1:3){
    test_area = map_matrix[(50*(vert-1) +1):(50*vert),(50*(horiz-1)+1):(50*horiz)]
    if(all(test_area == -1)){
      visualisation[vert,horiz] = -1
    } else if (!any(test_area == -1)){
      visualisation[vert,horiz] = 1
    }
  }
}
visualisation
# Let the map look like
# X12 
# X3X
# 45X
# 6XX

valid_positions = which(map_matrix != -1, arr.ind = T)
blank_matrix = map_matrix * NA
horizontal_positions = blank_matrix
horizontal_positions[valid_positions] = valid_positions[,2]
vertical_positions = blank_matrix
vertical_positions[valid_positions] = valid_positions[,1]
one_direction = blank_matrix
one_direction[valid_positions] = 1
next_position_matrices = list()
next_position_matrices$R$horizontal = horizontal_positions + 1
next_position_matrices$R$vertical = vertical_positions
next_position_matrices$R$direction = one_direction * 0
next_position_matrices$L$horizontal = horizontal_positions - 1
next_position_matrices$L$vertical = vertical_positions
next_position_matrices$L$direction = one_direction * 2
next_position_matrices$D$horizontal = horizontal_positions
next_position_matrices$D$vertical = vertical_positions + 1
next_position_matrices$D$direction = one_direction * 1
next_position_matrices$U$horizontal = horizontal_positions
next_position_matrices$U$vertical = vertical_positions - 1
next_position_matrices$U$direction = one_direction * 3

#The edges we get for free are 1-2, 1-3, 3-5, 4-5 and 4-6. That leaves 7 of the 12 edges of a cube - each of which needs to be dealt with from two sides
# 1) 1 top left = 6 top left, 1 top right = 6 bottom left
# 2) 1 top left = 4 bottom right, 1 bottom left = 4 top left
# 3) 2 top left = 6 bottom left, 2 top right = 6 bottom right
# 4) 2 top right = 5 bottom right, 2 bottom right = 5 top right
# 5) 2 bottom left = 3 top right, 2 bottom right = 3 bottom right
# 6) 3 top left = 4 top left, 3 bottom left = 4 top right
# 7) 5 bottom left = 6 top right, 5 bottom right = 6 bottom right

# 1)
# up from 1
next_position_matrices$U$horizontal[1,51:100] = 1
next_position_matrices$U$vertical[1,51:100] = 151:200
next_position_matrices$U$direction[1,51:100] = 0
# left from 6
next_position_matrices$L$horizontal[151:200,1] = 51:100
next_position_matrices$L$vertical[151:200,1] = 1
next_position_matrices$L$direction[151:200,1] = 1

# 2)
# left from 1
next_position_matrices$L$horizontal[1:50,51] = 1
next_position_matrices$L$vertical[1:50,51] = 150:101
next_position_matrices$L$direction[1:50,51] = 0

# left from 4
next_position_matrices$L$horizontal[101:150,1] = 51
next_position_matrices$L$vertical[101:150,1] = 50:1
next_position_matrices$L$direction[101:150,1] = 0

# 3)
# Up from 2
next_position_matrices$U$horizontal[1,101:150] = 1:50
next_position_matrices$U$vertical[1,101:150] = 200
next_position_matrices$U$direction[1,101:150] = 3

# Down from 6
next_position_matrices$D$horizontal[200, 1:50] = 101:150
next_position_matrices$D$vertical[200, 1:50] = 1
next_position_matrices$D$direction[200, 1:50] = 1

# 4)
# Right from 2
next_position_matrices$R$horizontal[1:50, 150] = 100
next_position_matrices$R$vertical[1:50, 150] = 150:101
next_position_matrices$R$direction[1:50, 150] = 2

# Right from 5
next_position_matrices$R$horizontal[101:150, 100] = 150
next_position_matrices$R$vertical[101:150, 100] = 50:1
next_position_matrices$R$direction[101:150, 100] = 2

# 5)
# Down from 2
next_position_matrices$D$horizontal[50, 101:150] = 100
next_position_matrices$D$vertical[50, 101:150] = 51:100
next_position_matrices$D$direction[50, 101:150] = 2

# Right from 3
next_position_matrices$R$horizontal[51:100, 100] = 101:150
next_position_matrices$R$vertical[51:100, 100] = 50
next_position_matrices$R$direction[51:100, 100] = 3

# 6)
# Left from 3
next_position_matrices$L$horizontal[51:100,51] = 1:50
next_position_matrices$L$vertical[51:100,51] = 101
next_position_matrices$L$direction[51:100,51] = 1

# Up from 4
next_position_matrices$U$horizontal[101,1:50] = 51
next_position_matrices$U$vertical[101,1:50] = 51:100
next_position_matrices$U$direction[101,1:50] = 0

# 7)
# Down from 5
next_position_matrices$D$horizontal[150, 51:100] = 50
next_position_matrices$D$vertical[150, 51:100] = 151:200
next_position_matrices$D$direction[150, 51:100] = 2

# Right from 6
next_position_matrices$R$horizontal[151:200, 50] = 51:100
next_position_matrices$R$vertical[151:200, 50] = 150
next_position_matrices$R$direction[151:200, 50] = 3

cube_single_step_path = directions

cube_end_of_step_horizontal_position = integer()
cube_end_of_step_vertical_position = integer()
cube_end_of_step_facing = integer()
for(instruction_index in 1:nrow(single_step_path)){
  if(instruction_index == 1){
    horizontal_position = starting_point_horizontal
    vertical_position = starting_point_vertical
    facing = 0
  }
  number = single_step_path$distance[instruction_index]
  for(i in 1:number){
        direction_letter = c('R','D','L','U')[facing + 1]
        suggested_horizontal = next_position_matrices[[direction_letter]]$horizontal[vertical_position, horizontal_position]
        suggested_vertical = next_position_matrices[[direction_letter]]$vertical[vertical_position, horizontal_position]
        suggested_facing = next_position_matrices[[direction_letter]]$direction[vertical_position, horizontal_position]
        if(map_matrix[suggested_vertical, suggested_horizontal] == 0){
          horizontal_position = suggested_horizontal
          vertical_position = suggested_vertical
          facing = suggested_facing
        }
  }
  if(single_step_path$turn[instruction_index] == 'R'){
    facing = (facing + 1) %% 4
  } else if(single_step_path$turn[instruction_index] == 'L'){
    facing = (facing - 1) %% 4
  }
  cube_end_of_step_horizontal_position = c(cube_end_of_step_horizontal_position, horizontal_position)
  cube_end_of_step_vertical_position = c(cube_end_of_step_vertical_position, vertical_position)
  cube_end_of_step_facing = c(cube_end_of_step_facing, facing)
}
cube_single_step_path$id = 1:nrow(cube_single_step_path)
cube_single_step_path$ss_end_horizontal = cube_end_of_step_horizontal_position
cube_single_step_path$ss_end_vertical = cube_end_of_step_vertical_position
cube_single_step_path$ss_end_facing = cube_end_of_step_facing

with(cube_single_step_path[nrow(cube_single_step_path),],
     1000 * ss_end_vertical + 4 * ss_end_horizontal + ss_end_facing)
#122153
