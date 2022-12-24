input_lines = readLines('./inputs/24.txt')
input_matrix = lines_to_matrix(input_lines)
dim(input_matrix)-2
# Common period is 600, so we could do pathfinding in a 25x120x600 space...
25*120*600
# 1.8M is possible
# There are no blizzards moving up/down on the entry/exit columns

# Today is the day of the cross join

vertical_height = dim(input_matrix)[1]
horizontal_width = dim(input_matrix)[2]

blizzard_locations = which(matrix(input_matrix %in% c('^', 'v', '<', '>'),
                                  nrow = 27, byrow = F), arr.ind = T)
table(input_matrix[blizzard_locations]) #Worth remembering that R matrices are stored column-first

# Let's use igraph for distance calculations

blizzard_df = data.frame(blizzardid = 1:nrow(blizzard_locations),
                         direction = input_matrix[blizzard_locations],
                         row = blizzard_locations[,1], 
                         col = blizzard_locations[,2])

valid_squares = data.frame(row = 1:vertical_height) %>%
  inner_join(data.frame(col = 1:horizontal_width), by = character()) %>%
  filter(col != 1 & col != horizontal_width) %>%
  filter(row %in% c(2:(vertical_height - 1)) | 
           (row == 1 & col == 2) |
           (row == vertical_height & col == horizontal_width - 1)) %>%
  mutate(coords_2d_char = paste0(row, ',', col))

edges_between_valid_squares = #includes loops
  valid_squares %>%
  inner_join(valid_squares %>%
               transmute(new_row = row, new_col = col),
             by = character()) %>%
  filter((abs(new_row - row) + abs(new_col - col)) <= 1)

# Initial time is T + 0

blizzard_coordinates = blizzard_df %>%
  rename(starting_row = row, starting_col = col) %>%
  inner_join(data.frame(time_mod_600 = 0:599),
             by = character()) %>%
  mutate(row_by_time = ifelse(direction %in% c('<', '>'),
                              0,
                              ifelse(direction == 'v', 1, -1)),
         col_by_time = ifelse(direction %in% c('^', 'v'),
                              0,
                              ifelse(direction == '>', 1, -1))) %>%
  mutate(row = (starting_row + row_by_time * time_mod_600 - 2) %% (vertical_height - 2) + 2,
         col = (starting_col + col_by_time * time_mod_600 - 2) %% (horizontal_width - 2) + 2) %>%
  mutate(coords_3d = paste0(row, ',', col, ',', time_mod_600))

forbidden_coordinates = unique(blizzard_coordinates$coords_3d)

graph_edges_proto = 
  edges_between_valid_squares %>%
  select(-coords_2d_char) %>%
  inner_join(data.frame(time_mod_600 = 0:599),
             by = character()) %>%
  mutate(new_time_mod_600 = (time_mod_600 + 1) %% 600) %>%
  mutate(coords_3d = paste0(row, ',', col, ',', time_mod_600),
         new_coords_3d = paste0(new_row, ',', new_col, ',', new_time_mod_600)) %>%
  filter(!coords_3d %in% forbidden_coordinates, !new_coords_3d %in% forbidden_coordinates)

traversal_graph = igraph::graph_from_data_frame(
  graph_edges_proto %>%
    transmute(from = coords_3d, to = new_coords_3d),
  directed = TRUE, vertices = NULL)

distances_from_start = distances(traversal_graph, v = c('1,2,0'),
                                 mode = 'out')[1,]
end_vertices = paste0(vertical_height,',',horizontal_width - 1,',',0:599)
distances_to_end = distances_from_start[end_vertices]
min(distances_to_end)
which.min(distances_to_end) 
#245
#27,121,245

start_vertices = paste0(1,',',2,',',0:599)
distances_back_to_start = distances(traversal_graph, v = '27,121,245',
                                    to = start_vertices, mode = 'out')[1,]
min(distances_back_to_start)
which.min(distances_back_to_start)
#283
#1,2,528

distances_back_to_end = distances(traversal_graph, v = '1,2,528',
                                  to = end_vertices, mode = 'out')[1,]
min(distances_back_to_end)
which.min(distances_back_to_end)
#270
#27,121,198
198 + 600
245 + 283 + 270
#798