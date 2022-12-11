input_lines = readLines('./inputs/11.txt')
monkey_count = (length(input_lines) + 1)/7
# Each monkey has id, fun, test_quotient, true_target, false_target
# Monkey ID n is list entry n + 1
monkeys = list()
expression_to_function = 
  function(expr){
    f = function(old){}
    body(f) = expr
    f
  }

for(monkey_id in 0:(monkey_count - 1)){
  monkeys[[monkey_id + 1]] = 
    list(
      id = monkey_id,
      items = as.numeric(el(strsplit(
        gsub('  Starting items: ', '', input_lines[monkey_id * 7 + 2])
      , ','))),
      fun = expression_to_function(
        parse(text = gsub('  Operation: new = ', '', input_lines[monkey_id * 7 + 3]))
      ),
      test_quotient = as.numeric(
        gsub('  Test: divisible by (\\d+)', '\\1', input_lines[monkey_id * 7 + 4])
      ),
      true_target = as.numeric(
        gsub('    If true: throw to monkey (\\d+)', '\\1', input_lines[monkey_id * 7 + 5])
      ),
      false_target = as.numeric(
        gsub('    If false: throw to monkey (\\d+)', '\\1', input_lines[monkey_id * 7 + 6])
      )
      
    )
}

working_items_per_monkey = lapply(monkeys,
                                  function(monkey){monkey$items})


# Data to gather:
# items inspected per monkey
inspected_items = rep(0, monkey_count)
for(round in 1:20){
  for(monkey_index in 1:monkey_count){
    worry_levels = working_items_per_monkey[[monkey_index]]
    working_items_per_monkey[[monkey_index]] = numeric(0)
    inspected_items[monkey_index] = inspected_items[monkey_index] + length(worry_levels)
    updated_worry_levels = monkeys[[monkey_index]]$fun(worry_levels)
    updated_worry_levels = updated_worry_levels %/% 3
    test_values = (updated_worry_levels %% monkeys[[monkey_index]]$test_quotient) == 0
    true_values = updated_worry_levels[test_values]
    false_values = updated_worry_levels[!test_values]
    
    true_monkey = monkeys[[monkey_index]]$true_target + 1
    false_monkey = monkeys[[monkey_index]]$false_target + 1
    
    working_items_per_monkey[[true_monkey]] = c(
      working_items_per_monkey[[true_monkey]], true_values
    )
    working_items_per_monkey[[false_monkey]] = c(
      working_items_per_monkey[[false_monkey]], false_values
    )
  }
}
prod(sort(inspected_items, decreasing = T)[1:2])
#120756

# No more division by 3
product_of_test_quotients = prod(sapply(monkeys,
       function(monkey){monkey$test_quotient}))
# we could modulo by this number

working_items_per_monkey = lapply(monkeys,
                                  function(monkey){monkey$items})
inspected_items = rep(0, monkey_count)
for(round in 1:10000){
  for(monkey_index in 1:monkey_count){
    worry_levels = working_items_per_monkey[[monkey_index]]
    working_items_per_monkey[[monkey_index]] = numeric(0)
    inspected_items[monkey_index] = inspected_items[monkey_index] + length(worry_levels)
    updated_worry_levels = monkeys[[monkey_index]]$fun(worry_levels)
    updated_worry_levels = updated_worry_levels %% product_of_test_quotients
    test_values = (updated_worry_levels %% monkeys[[monkey_index]]$test_quotient) == 0
    true_values = updated_worry_levels[test_values]
    false_values = updated_worry_levels[!test_values]
    
    true_monkey = monkeys[[monkey_index]]$true_target + 1
    false_monkey = monkeys[[monkey_index]]$false_target + 1
    
    working_items_per_monkey[[true_monkey]] = c(
      working_items_per_monkey[[true_monkey]], true_values
    )
    working_items_per_monkey[[false_monkey]] = c(
      working_items_per_monkey[[false_monkey]], false_values
    )
  }
}
prod(sort(inspected_items, decreasing = T)[1:2])
#39109444654