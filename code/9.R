input_lines = readLines('./inputs/9.txt')
instructions = data.frame(direction = split_vectorised(input_lines, ' ', 1),
                          distance = as.numeric(split_vectorised(input_lines, ' ', 2)))
sum(instructions$distance) #11510 steps is small enough that a naive approach should work

# Simulate all steps individually
# Start at the origin

head_position = c(0,0)
tail_position = c(0,0)
head_positions = data.frame(time = 0, step = 0, x = 0, y = 0)
tail_positions = data.frame(time = 0, step = 0, x = 0, y = 0)
time = 0
for(instruction_index in 1:nrow(instructions)){
  instruction_direction = instructions$direction[instruction_index]
  instruction_distance = instructions$distance[instruction_index]
  for(sub_move_index in 1:instruction_distance){
    time = time + 1
    if(instruction_direction == 'L'){
      head_position[1] = head_position[1] - 1
    }
    if(instruction_direction == 'R'){
      head_position[1] = head_position[1] + 1
    }
    if(instruction_direction == 'U'){
      head_position[2] = head_position[2] + 1
    }
    if(instruction_direction == 'D'){
      head_position[2] = head_position[2] - 1
    }
    head_positions = rbind(head_positions,
                           data.frame(time = time, 
                                      step = instruction_index, 
                                      x = head_position[1], 
                                      y = head_position[2]))
    if(max(abs(head_position - tail_position)>1)){
      tail_position = tail_position + sign(head_position - tail_position)
    }
    tail_positions = rbind(tail_positions,
                           data.frame(time = time, 
                                      step = instruction_index, 
                                      x = tail_position[1], 
                                      y = tail_position[2]))
  }
}

nrow(unique(tail_positions[,c('x','y')]))
#6266


# Function for simulating knot n+1 from knot n
get_next_knot_positions = function(initial_knot_positions){
  time_vector = initial_knot_positions$time
  step_vector = initial_knot_positions$step
  x_vector = initial_knot_positions$x
  y_vector = initial_knot_positions$y
  new_x_vector = x_vector * 0
  new_y_vector = y_vector * 0
  subsequent_knot_position = c(0,0)
  for(rownum in time_vector + 1){#time_vector starts at 0
    initial_knot_position = c(x_vector[rownum], y_vector[rownum])
    if(max(abs(initial_knot_position - subsequent_knot_position)>1)){
      subsequent_knot_position = subsequent_knot_position + 
        sign(initial_knot_position - subsequent_knot_position)
    }
    new_x_vector[rownum] = subsequent_knot_position[1]
    new_y_vector[rownum] = subsequent_knot_position[2]
  }
  return(data.frame(time = time_vector, step = step_vector, x = new_x_vector, y = new_y_vector))
}

#test it
knot_1_positions = get_next_knot_positions(head_positions)
all.equal(knot_1_positions, tail_positions) # TRUE

subsequsent_knot_positions = list()
subsequent_knot_positions[[1]] = knot_1_positions
for(knot_index in 2:9){
  subsequent_knot_positions[[knot_index]] = get_next_knot_positions(subsequent_knot_positions[[knot_index - 1]])
}
nrow(unique(subsequent_knot_positions[[9]][,c('x','y')]))
#2369