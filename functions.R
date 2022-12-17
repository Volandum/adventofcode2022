split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})

print_large_number = function(number){
  format(number, scientific = F)
}
