input_lines = readLines('./inputs/8.txt')
matrix_dimension = length(input_lines)
input_matrix = matrix(as.numeric(unlist(strsplit(input_lines, ''))), ncol = matrix_dimension, byrow = TRUE)


columnar_cummax_with_offset = function(mat){
  temp = apply(mat, 2, cummax)
  spacer = matrix(rep(-1, matrix_dimension), ncol = matrix_dimension, byrow = TRUE)
  return(rbind(spacer, temp[1:(matrix_dimension - 1),]))
}
max_from_top = columnar_cummax_with_offset(input_matrix)
max_from_bottom = columnar_cummax_with_offset(input_matrix[matrix_dimension:1,])[matrix_dimension:1,]
max_from_left = t(columnar_cummax_with_offset(t(input_matrix)))
max_from_right = t(columnar_cummax_with_offset(t(input_matrix[,matrix_dimension:1])))[,matrix_dimension:1]

invisible_at_or_below_this_height = pmin(max_from_top, max_from_bottom, max_from_left, max_from_right) 
sum(input_matrix > invisible_at_or_below_this_height)
# 1814

visibility_to_start_of_vector = function(input_vector){
  last_at_height = rep(1, 10) #position of last tree at height X - 1, edge trees have all heights
  output = input_vector * 0
  for(position in 1:matrix_dimension){
    tree_height = input_vector[position]
    last_blocker = max(last_at_height[(tree_height + 1):10])
    visibility = position - last_blocker
    output[position] = visibility
    last_at_height[tree_height + 1] = position
  }
  return(output)
}

columnar_visibility = function(mat){
  return(apply(mat, 2, visibility_to_start_of_vector))
}

visibility_up = columnar_visibility(input_matrix)
visibility_down = columnar_visibility(input_matrix[matrix_dimension:1,])[matrix_dimension:1,]
visibility_left = t(columnar_visibility(t(input_matrix)))
visibility_right = t(columnar_visibility(t(input_matrix[,matrix_dimension:1])))[,matrix_dimension:1]

# * is pointwise multiplication
total_visibility = visibility_up * visibility_down * visibility_left * visibility_right
max(total_visibility)
# 330786