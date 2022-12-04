input_lines = readLines('./inputs/4.txt')

data_wide = data.frame(pairid = 1:1000, 
                       e1start = as.numeric(split_vectorised(input_lines, '[,-]', 1)),
                       e1end = as.numeric(split_vectorised(input_lines, '[,-]', 2)),
                       e2start = as.numeric(split_vectorised(input_lines, '[,-]', 3)),
                       e2end = as.numeric(split_vectorised(input_lines, '[,-]', 4)))

data_wide %>% mutate(e1_in_e2 = e1start >= e2start & e1end <= e2end,
                     e2_in_e1 = e2start >= e1start & e2end <= e1end) %>%
  count(e1_in_e2 | e2_in_e1)
#456

data_wide %>%
  mutate(overlap = e1start <= e2end & e2start <= e1end) %>%
  count(overlap)
#808