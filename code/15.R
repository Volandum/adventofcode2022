input_lines = readLines('./inputs/15.txt')
input_data = 
  data.frame(
    sensor_x = as.numeric(split_vectorised(input_lines, '[=:,]', 2)),
    sensor_y = as.numeric(split_vectorised(input_lines, '[=:,]', 4)),
    beacon_x = as.numeric(split_vectorised(input_lines, '[=:,]', 6)),
    beacon_y = as.numeric(split_vectorised(input_lines, '[=:,]', 8))
  )
input_data$distance = abs(input_data$sensor_x - input_data$beacon_x) +
  abs(input_data$sensor_y - input_data$beacon_y)

# For row y = 2000000 try to assemble the set of integer ranges that have been scanned
test_row = 2000000
row_2m_data = 
  input_data %>%
  filter(test_row <= sensor_y + distance & test_row >= sensor_y - distance) %>%
  mutate(x_range = distance - abs(test_row - sensor_y)) %>%
  mutate(xmin = sensor_x - x_range, xmax = sensor_x + x_range) %>%
  arrange(xmin)

union_of_ranges = function(xmin, xmax){
  xmin_sorted = xmin[order(xmin)]
  xmax_sorted = xmax[order(xmin)]
  output = list()
  current_min = NA
  current_max = NA
  for(i in 1:length(xmin_sorted)){
    new_min = xmin_sorted[i]
    new_max = xmax_sorted[i]
    if(i == 1){
      current_min = new_min
      current_max = new_max
    } else if (new_min <= current_max){
      current_max = max(current_max, new_max)
    } else {
      output = c(output, list(c(current_min, current_max)))
      current_min = new_min
      current_max = new_max
    }
    
    if(i == length(xmin_sorted)){
      output = c(output, list(c(current_min, current_max)))
    }
  }
  return(output)
}

row_2m_ranges = union_of_ranges(row_2m_data$xmin, row_2m_data$xmax)

get_size_of_union = function(ranges){
  sum(sapply(row_2m_ranges, \(x){x[2]-x[1] + 1}))
}
get_size_of_union(row_2m_ranges) - 
  (input_data %>% filter(beacon_y == test_row) %>% pull(beacon_x) %>% unique %>% length)
#5367037

# union_of_ranges_and_get_overlap = function(xmin, xmax){
#   xmin_sorted = xmin[order(xmin)]
#   xmax_sorted = xmax[order(xmin)]
#   output = list()
#   current_min = NA
#   current_max = NA
#   last_min = NA
#   last_max = NA
#   overlaps = numeric()
#   for(i in 1:length(xmin_sorted)){
#     new_min = xmin_sorted[i]
#     new_max = xmax_sorted[i]
#     if(i == 1){
#       current_min = new_min
#       current_max = new_max
#     } else if (new_min <= current_max){
#       current_max = max(current_max, new_max)
#       overlap = pmin(new_max, last_max) - pmax(new_min, last_min) + 1
#       if(overlap < 1){browser()}
#       overlaps = c(overlaps, overlap)
#     } else {
#       output = c(output, list(c(current_min, current_max)))
#       current_min = new_min
#       current_max = new_max
#     }
#     last_min = new_min
#     last_max = new_max
#     if(i == length(xmin_sorted)){
#       output = c(output, list(c(current_min, current_max)))
#     }
#   }
#   return(list(ranges = output, overlaps = overlaps))
# }


get_ranges = function(y_value){
  range_data = 
    input_data %>%
    filter(y_value <= sensor_y + distance & y_value >= sensor_y - distance) %>%
    mutate(x_range = distance - abs(y_value - sensor_y)) %>%
    mutate(xmin = sensor_x - x_range, xmax = sensor_x + x_range) %>%
    arrange(xmin)
  union_range = union_of_ranges(range_data$xmin, range_data$xmax)
  return(list(ranges = union_range, sensor_table = range_data))
}
bounds = 4000000
working_y = 0


while(working_y <= bounds){
  range_data = get_ranges(working_y)
  # message(range_data$ranges$ranges)
  if(!(el(range_data$ranges)[1]< 0 &
     el(range_data$ranges)[2] > bounds)){
    break
  }
  overlaps = range_data$sensor_table %>% inner_join(x = ., y = ., by = character()) %>%
    filter(xmin.y <= xmax.x & xmin.y >= xmin.x) %>%
    mutate(overlap = pmin(xmax.x, xmax.y) - pmax(xmin.x, xmin.y) + 1) %>%
    mutate(overlap_changes_at = 
             case_when(sensor_y.x <= working_y & sensor_y.y <= working_y ~
                         working_y + ceiling(overlap/2),
                       sensor_y.x > working_y & sensor_y.y > working_y ~
                         pmin(sensor_y.x, sensor_y.y),
                       TRUE ~ #one of each
                         pmin(sensor_y.x + distance.x + 1,
                              sensor_y.y + distance.y + 1)
                    ))
  if(min(overlaps$overlap_changes_at) <= working_y){
    stop()
  }
  working_y = min(overlaps$overlap_changes_at)
  message(working_y)
}
range_data
y = working_y
x = range_data$ranges[[1]][2] + 1
frequency = x * 4000000 + y
format(frequency, scientific = F)
# 11914583249288