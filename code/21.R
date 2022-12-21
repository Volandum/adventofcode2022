input_lines = readLines('./inputs/21.txt')

# We have a tree of monkeys from root and need to work up

parsed_input_lines = list()
for(line in input_lines){
  if(grepl('\\d+', line)){
    monkey_name = split_vectorised(line, ':', 1)
    monkey_number = as.numeric(get_regex_match(line, '(\\d+)'))
    parsed_input_lines[[monkey_name]] = monkey_number
  } else {
    monkey_name = split_vectorised(line, ':', 1)
    operation_char = get_regex_match(line, '([*+-/])')
    operation = eval(str2expression(paste0('`', operation_char, '`')))
    operand_1_name = get_regex_match(line, ': (\\w+)')
    operand_2_name = get_regex_match(line, '(\\w+)$')
    parsed_input_lines[[monkey_name]] = list(operation = operation,
                                           operand_1 = operand_1_name,
                                           operand_2 = operand_2_name)
  }
}

evaluate_monkey = function(monkey_name){
  rule = parsed_input_lines[[monkey_name]]
  if(is.numeric(rule)){
    return(rule)
  }
  return(rule$operation(evaluate_monkey(rule$operand_1), evaluate_monkey(rule$operand_2)))
}

print_large_number(evaluate_monkey('root'))
#80326079210554

parsed_input_lines$root
# Part 2: make jsrw = ptvl
library('gmp')
#assume we never have x on the denominator so this is just linear
#represent expressions as c(a,b) = a + bx
evaluate_as_expression = function(monkey_name){
  if(monkey_name == 'humn'){
    return(c(as.bigz(0), as.bigz(1)))
  }
  rule = parsed_input_lines[[monkey_name]]
  if(is.numeric(rule)){
    return(c(as.bigz(rule), as.bigz(0)))
  }
  operand1 = evaluate_as_expression(rule$operand_1)
  operand2 = evaluate_as_expression(rule$operand_2)
  if(identical(rule$operation, `+`)){
    return(operand1 + operand2)
  }
  if(identical(rule$operation, `-`)){
    return(operand1 - operand2)
  }
  if(identical(rule$operation, `*`)){
    if(operand1[2] != 0 & operand2[2] !=0){
      stop('Degree too high')
    }
    return(c(operand1[1] * operand2[1],
             operand1[2] * operand2[1] + operand1[1] * operand2[2]))
  }
  if(identical(rule$operation, `/`)){
    if(operand2[2] != 0){
      stop('Rational fraction!')
    }
    return(c(operand1[1]/operand2[1], operand1[2]/operand2[1]))
  }
  stop('Unknown operation')
}

difference = evaluate_as_expression('jsrw') - evaluate_as_expression('ptvl')
# I think I have precision issues

# 4617x/616 == 8351261809064613/308
# x = 8351261809064613/308/(4617/616)

-difference[1]/difference[2]
#3617613952378