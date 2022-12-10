input_lines = readLines('./inputs/10.txt')
number_of_instructions = length(input_lines)
# Capture cycle_number, instruction_number, x_at_start, x_at_end, instruction1 (noop, addx1, addx2), instruction2 (the add number)
x = 1
step = 1
register_states = list()
for(instruction_index in 1:number_of_instructions){
  instruction = input_lines[instruction_index]
  if(instruction == 'noop'){
    register_states[[instruction_index]] = 
      data.frame(cycle_number = step, 
                 instruction_number = instruction_index,
                 x_at_start = x,
                 x_at_end = x,
                 instruction1 = 'noop',
                 instruction2 = NA_integer_)
    step = step + 1
  } else {
    increment_number = as.numeric(el(strsplit(instruction, ' '))[2])
    new_x = x + increment_number
    register_states[[instruction_index]] = 
      data.frame(cycle_number = c(step, step + 1), 
                 instruction_number = c(instruction_index, instruction_index),
                 x_at_start = c(x,x),
                 x_at_end = c(x, new_x),
                 instruction1 = c('addx1', 'addx2'),
                 instruction2 = c(increment_number, increment_number))
    step = step + 2
    x = new_x
  }
}

register_states_dataframe = data.table::rbindlist(register_states)

register_states_dataframe %>% filter(cycle_number %in% c(20, 60, 100, 140, 180, 220)) %>%
  mutate(signal_strength = cycle_number * x_at_start) %>%
  pull(signal_strength) %>% sum
#13480

register_states_dataframe %>% mutate(y_coordinate = (cycle_number - 1) %/% 40,
                                     x_coordinate = (cycle_number - 1) %% 40 + 1) %>%
  mutate(character = ifelse((x_at_start - x_coordinate) %in% c(-2, -1, 0),
                            '#', '.')) -> characters_on_screen

characters_on_screen %>% group_by(y_coordinate) %>% summarise(line = paste0(character, collapse = ''))
#EGJBGCFK
