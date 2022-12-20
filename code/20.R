input_lines = readLines('./inputs/20.txt')
input_vector = as.numeric(input_lines)
#input_vector = c(1,2,-3,3,-2,0,4)

number_of_items = length(input_vector)

# Each item should have an ID as well
# To move position a to position b:
# b > a: a:x -> b, (a,b]: x -> x - 1
# b == a: nothing
# b < a: a:x -> b, [b,a): x -> x + 1

position_vector = 1:number_of_items
move_item = function(item_index, position_vector){
  item = input_vector[item_index]
  #print(item)
  starting_position = which(position_vector == item_index)
  #print(starting_position)
  end_position = (starting_position + item - 2) %% (number_of_items - 1) + 2
  #print(end_position)
  if(starting_position < end_position){
    temp_vector = position_vector[(starting_position + 1):end_position]
    position_vector[starting_position:end_position] = c(temp_vector, item_index)
  } else if(starting_position > end_position){
    temp_vector = position_vector[end_position:(starting_position - 1)]
    position_vector[end_position:starting_position] = c(item_index, temp_vector)
  }
  position_vector
}
for(index in 1:number_of_items){
  position_vector = move_item(index, position_vector)
  #print(input_vector[position_vector])
}
