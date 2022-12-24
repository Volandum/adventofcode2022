split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

print_large_number = function(number){
  format(number, scientific = F)
}

get_regex_match = Vectorize(function(text, pattern, position = 1){
  unlist(regmatches(text, regexec(pattern, text, perl = T)))[position + 1]
})

lines_to_matrix = function(lines){
  matrix(unlist(strsplit(lines, '')),
         nrow = length(lines), byrow = T)
}
