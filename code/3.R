input_lines = readLines('./inputs/3.txt')

letter_to_priority = function(letter){
  mapping = c(letters, toupper(letters))
  match(letter, mapping)
}

line_to_common_letter = function(line){
  part1 = substr(line,1,nchar(line)/2)
  part2 = substr(line, nchar(line)/2 + 1, nchar(line))
  part1_letters = el(strsplit(part1, ''))
  part2_letters = el(strsplit(part2, ''))
  intersect(part1_letters, part2_letters)
}

line_to_common_letter('jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL')

input_lines %>%
  sapply(line_to_common_letter) %>%
  letter_to_priority() %>%
  sum
# 7817

priority_sum = 0
string_to_letters = function(string){el(strsplit(string, ''))}
for(i in 1:100){
  elf1_letters = string_to_letters(input_lines[i*3 - 2])
  elf2_letters = string_to_letters(input_lines[i*3 - 1])
  elf3_letters = string_to_letters(input_lines[i*3 - 0])
  common_letter = intersect(intersect(elf1_letters, elf2_letters), elf3_letters)
  priority_sum = priority_sum + letter_to_priority(common_letter)
}
#2444