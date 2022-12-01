input_lines = readLines('./inputs/1.txt')
calorie_data = data.frame(elfid = cumsum(input_lines == ''), calories = input_lines) %>%
  filter(calories != "") %>%
  mutate(calories = as.numeric(calories))

calorie_data %>% group_by(elfid) %>% summarise(total_calories = sum(calories)) %>%
  arrange(desc(total_calories))

# Elf 52 with 69289 calories

calorie_data %>% group_by(elfid) %>% summarise(total_calories = sum(calories)) %>%
  arrange(desc(total_calories)) %>% slice_head(n = 3) %>%
  pull(total_calories) %>% sum

# 205615