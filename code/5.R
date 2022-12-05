input_lines = readLines('./inputs/5.txt')
split_point = which(input_lines == '')

instructions = input_lines[(split_point + 1):length(input_lines)]
stacks = list()
for(i in 1:9){
  proto_stack = c()
  for(j in 8:1){
    potential_letter = substr(input_lines[j], 4 * i - 2, 4*i - 2)
    if(potential_letter != ' '){
      proto_stack = c(proto_stack, potential_letter)
    }
  }
  stacks[[i]] = proto_stack
}
starting_stacks = stacks

instructions_table = data.frame(id = 1:length(instructions),
                                number = as.numeric(split_vectorised(instructions, ' ', 2)),
                                start = as.numeric(split_vectorised(instructions, ' ', 4)),
                                end = as.numeric(split_vectorised(instructions, ' ', 6)))

crane = function(stacks, start, end, number){
  moving_items = stacks[[start]][(length(stacks[[start]])-number + 1):length(stacks[[start]])]
  stacks[[start]] = stacks[[start]][1:(length(stacks[[start]])-number)]
  stacks[[end]] = c(stacks[[end]], rev(moving_items))
  return(stacks)
}

stacks = starting_stacks
for(i in 1:nrow(instructions_table)){
  stacks = crane(stacks, 
                 instructions_table$start[i], 
                 instructions_table$end[i],
                 instructions_table$number[i])
}
paste0(sapply(stacks, function(v){rev(v)[1]}), collapse = '')

crane1 = function(stacks, start, end, number){
  moving_items = stacks[[start]][(length(stacks[[start]])-number + 1):length(stacks[[start]])]
  stacks[[start]] = stacks[[start]][1:(length(stacks[[start]])-number)]
  stacks[[end]] = c(stacks[[end]], moving_items)
  return(stacks)
}
stacks = starting_stacks
for(i in 1:nrow(instructions_table)){
  stacks = crane1(stacks, 
                 instructions_table$start[i], 
                 instructions_table$end[i],
                 instructions_table$number[i])
}
paste0(sapply(stacks, function(v){rev(v)[1]}), collapse = '')
