input = readLines('./inputs/6.txt')
input_characters = el(strsplit(input, ''))
for(i in 4:length(input_characters)){
  if(length(unique(input_characters[(i-3):i])) == 4) break
}
i #1655

for(i in 14:length(input_characters)){
  if(length(unique(input_characters[(i-13):i])) == 14) break
}
i #2665
