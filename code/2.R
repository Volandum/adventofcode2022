input_plays = read.table('./inputs/2.txt')
strategy_table = data.frame(id = 1:nrow(input_plays), P1 = input_plays$V1, P2 = input_plays$V2)
outcomes = data.frame(P1 = c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C'),
                      P2 = c('X', 'Y', 'Z', 'X', 'Y', 'Z', 'X', 'Y', 'Z'),
                      outcome = c(3, 6, 0, 0, 3, 6, 6, 0, 3),
                      shapescore = c(1, 2, 3, 1, 2, 3, 1, 2, 3))
expected = merge(strategy_table, outcomes) %>% arrange(id)
sum(expected$outcome) + sum(expected$shapescore) #12772


actual_strategy_table = data.frame(id = 1:nrow(input_plays), P1 = input_plays$V1, 
                                   outcome = c(ifelse(input_plays$V2 == 'X', 0,
                                                      ifelse(input_plays$V2 == 'Y', 3, 6))))
actual_expected = merge(actual_strategy_table, outcomes) %>% arrange(id)
sum(actual_expected$outcome) + sum(actual_expected$shapescore) #11618
