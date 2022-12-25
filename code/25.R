input_lines = readLines('./inputs/25.txt')
snafu_to_bigint = function(snafu_text){
  if(nchar(snafu_text) == 0){
    stop('empty string')
  } else if (nchar(snafu_text) == 1){
    translation = c(
      `2` = 2,
      `1` = 1,
      `0` = 0,
      `-` = -1,
      `=` = -2
    )
    return(as.bigz(translation[snafu_text]))
  } else {
    return(snafu_to_bigint(substr(snafu_text,1,(nchar(snafu_text)-1))) * 5 +
             snafu_to_bigint(substr(snafu_text,nchar(snafu_text),nchar(snafu_text))))
  }
}

bigint_to_snafu = function(input_integer){
  if(input_integer >= -2 & input_integer <= 2){
    return(c('=','-','0','1','2')[as.integer(input_integer + 3)])
  } else {
    residue_mod_5 = input_integer %% 5
    if(residue_mod_5 <= 2){
      small_part = residue_mod_5
    } else {
      small_part = residue_mod_5 - 5
    }
    big_part = (input_integer - small_part) %/% 5
    return(paste0(bigint_to_snafu(big_part), bigint_to_snafu(small_part)))
  }
}

fuel_needs = Vectorize(snafu_to_bigint)(input_lines)
total_fuel = Reduce(`+`, fuel_needs, as.bigz('0'))
bigint_to_snafu(total_fuel)
#2-2=12=1-=-1=000=222