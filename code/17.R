input_line = readLines('./inputs/17.txt')
input_stream = el(strsplit(input_line, ''))

rock_prototypes = list(
  cbind(y = c(1, 1, 1, 1),
       x = c(3, 4, 5, 6)),
  cbind(y =  c(2, 1, 2, 3, 2),
       x =c(3, 4, 4, 4, 5)),
  cbind(y = c(1, 1, 1, 2, 3),
       x = c(3, 4, 5, 5, 5)),
  cbind(y = c(1, 2, 3, 4),
       x = c(3, 3, 3, 3)),
  cbind(y = c(1, 2, 1, 2),
       x = c(3, 3, 4, 4))
)

rock_matrix = matrix(0,
                     nrow = 10000,
                     ncol = 7)

place_rock = function(rock_type){
  y_max = max(c(which(rock_matrix > 0, arr.ind = T)[,1], 0))
  rock = rock_prototypes[[rock_type]]
  rock[,1] = rock[,1] + y_max + 3
  return(rock)
}

displace_rock = function(rock, direction = 'D'){
  new_rock = rock
  if(direction == '<'){
    new_rock[,2] = new_rock[,2] - 1
  } else if(direction == '>'){
    new_rock[,2] = new_rock[,2] + 1
  } else if(direction == 'D'){
    new_rock[,1] = new_rock[,1] - 1
  }
  # Collision with walls
  if(any(new_rock[,2] < 1) |
     any(new_rock[,2] > 7) |
     any(new_rock[,1] < 1)){
    return(NULL)
  }
  # Collision with rocks
  if(any(rock_matrix[new_rock] > 0)){
    return(NULL)
  }
  return(new_rock)
}

current_input_position = 1
input_length = length(input_stream)
rock_matrix = matrix(0,
                     nrow = 10000,
                     ncol = 7)

for(rock_index in 1:2022){
  rock_type = (rock_index - 1) %% 5 + 1
  falling_rock = place_rock(rock_type)
  while(T){
    potential_rock_position = displace_rock(falling_rock,
                                            input_stream[current_input_position])
    current_input_position = current_input_position %% input_length + 1
    if(!is.null(potential_rock_position)){
      falling_rock = potential_rock_position
    }
    potential_rock_position = displace_rock(falling_rock, 'D')
    if(!is.null(potential_rock_position)){
      falling_rock = potential_rock_position
    } else {
      break
    }
  }
  rock_matrix[falling_rock] = rock_index
}
max(which(rock_matrix > 0, arr.ind = T)[,1])
#3085

# Search for periodic behaviour
current_input_position = 1
input_length = length(input_stream)
rock_matrix = matrix(0,
                     nrow = 20000,
                     ncol = 7)
search_iterations = 8000
input_positions = rep(0, search_iterations)
for(rock_index in 1:search_iterations){
  rock_type = (rock_index - 1) %% 5 + 1
  falling_rock = place_rock(rock_type)
  while(T){
    potential_rock_position = displace_rock(falling_rock,
                                            input_stream[(current_input_position - 1) %% input_length + 1])
    current_input_position = current_input_position + 1
    if(!is.null(potential_rock_position)){
      falling_rock = potential_rock_position
    }
    potential_rock_position = displace_rock(falling_rock, 'D')
    if(!is.null(potential_rock_position)){
      falling_rock = potential_rock_position
    } else {
      break
    }
  }
  rock_matrix[falling_rock] = rock_index
  input_positions[rock_index] = current_input_position
}

# After 1724, 3429, 5134 and 6839 rocks the next item in the input stream is #3
max(which(rock_matrix <= 1724 & rock_matrix > 0, arr.ind = T)[,1]) #2617
max(which(rock_matrix <= 3429 & rock_matrix > 0, arr.ind = T)[,1]) #5235
max(which(rock_matrix <= 5134 & rock_matrix > 0, arr.ind = T)[,1]) #7853
max(which(rock_matrix <= 6839 & rock_matrix > 0, arr.ind = T)[,1]) #10471
c(6839, 5134, 3429) - c(5134, 3429, 1724) #1705 steps
c(10471, 7853, 5235) - c(7853, 5235, 2617) #2618 height

# So we want to know how tall (10^12 mod 1705) + 1724 gets, 
# and add ((10 ^ 12 - 3309) / 1705) * 2618 to that
# the -2 because 1724 > 1705

# Let's do 5000 as a test
# We can skip 5000 %/% 1705 - 1 = 1 goes
# go to 5000 - (5000 %/% 1705 - 1) * 1705 = 3295 which has height 5015
# Predicted height = 5015 + 2618 = 7633
# Confirmed

# For 10^12 we want to skip
10^12 %/% 1705 - 1 #586510262 goes
# go to
10^12 - ((10^12 %/% 1705 - 1) * 1705) #3290

current_input_position = 1
input_length = length(input_stream)
rock_matrix = matrix(0,
                     nrow = 10000,
                     ncol = 7)

for(rock_index in 1:3290){
  rock_type = (rock_index - 1) %% 5 + 1
  falling_rock = place_rock(rock_type)
  while(T){
    potential_rock_position = displace_rock(falling_rock,
                                            input_stream[current_input_position])
    current_input_position = current_input_position %% input_length + 1
    if(!is.null(potential_rock_position)){
      falling_rock = potential_rock_position
    }
    potential_rock_position = displace_rock(falling_rock, 'D')
    if(!is.null(potential_rock_position)){
      falling_rock = potential_rock_position
    } else {
      break
    }
  }
  rock_matrix[falling_rock] = rock_index
}
max(which(rock_matrix > 0, arr.ind = T)[,1]) #5008


print_large_number((10^12 %/% 1705 - 1) * 2618 + 5008)
# 1535483870924
# Checked in Python in case of integer precision issues