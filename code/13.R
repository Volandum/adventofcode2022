input_lines = readLines('./inputs/13.txt')
number_of_pairs = (length(input_lines) + 1)/3
left_lines = input_lines[(1:number_of_pairs) * 3 - 2]
right_lines = input_lines[(1:number_of_pairs) * 3 - 1]

parse_packet = function(packet){
  eval(parse(text = gsub('[', 'list(', gsub(']',')',packet), fixed = T)))
}

compare_packets = function(left, right){
  # print(dput(left))
  # print(dput(right))
  # returns T if left < right, F if right > left and NA if left == right
  if(is.list(left) & is.list(right)){
    # both lists
    left_length = length(left)
    right_length = length(right)
    min_length = min(left_length, right_length)
    for(compare_index in seq_len(min_length)){
      item_comparison = compare_packets(left[[compare_index]], right[[compare_index]])
      if(!is.na(item_comparison)){
        return(item_comparison)
      }
    }
    if(left_length < right_length){
      return(TRUE)
    } else if(left_length > right_length){
      return(FALSE)
    } else {
      return(NA)
    }
  } else if(!is.list(left) & !is.list(right)){
    if(left == right){
      return(NA)
    } else {
      return(left < right)
    }
  } else if(!is.list(left) & is.list(right)){
    return(compare_packets(list(left), right))
  } else if(is.list(left) & !is.list(right)){
    return(compare_packets(left, list(right)))
  }
  stop('Error')
}

parse_and_compare = Vectorize(function(left_line, right_line){
  compare_packets(parse_packet(left_line), parse_packet(right_line))
})

comparisons = parse_and_compare(left_lines, right_lines)
sum(which(comparisons))
# 5390

all_lines = c(left_lines, right_lines)
# Just need to know how many are less than [[2]] and how many are less than [[6]]
less_than_decoder1 = sum(parse_and_compare(all_lines, '[[2]]'))
less_than_decoder2 = sum(parse_and_compare(all_lines, '[[6]]'))
decoder1_index = 1 + less_than_decoder1
decoder2_index = 2 + less_than_decoder2
decoder1_index * decoder2_index
# 19261