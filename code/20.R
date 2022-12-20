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

zero_position = which(position_vector == which(input_vector == 0))
positions_of_interest = zero_position + c(1000, 2000, 3000)
indexes_of_interest = position_vector[positions_of_interest]
items_of_interest = input_vector[indexes_of_interest]
sum(items_of_interest)
#2622


decryption_key = 811589153

input_vector = input_vector * decryption_key
position_vector = 1:number_of_items
print(input_vector[position_vector])
for(process in 1:10){
  for(index in 1:number_of_items){
    position_vector = move_item(index, position_vector)
    #print(input_vector[position_vector])
  }
  #print(input_vector[position_vector])
}
# display for test case doesn't match example but it's non-canonical representations (as the list is reorderable)

zero_position = which(position_vector == which(input_vector == 0))
positions_of_interest = (zero_position + c(1000, 2000, 3000) - 1) %% number_of_items + 1
indexes_of_interest = position_vector[positions_of_interest]
items_of_interest = input_vector[indexes_of_interest]
print_large_number(sum(items_of_interest))
#1538773034088