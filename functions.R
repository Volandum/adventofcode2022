split_vectorised = Vectorize(function(text, separators, position){
  unlist(strsplit(text, separators))[position]
})
