input_lines = readLines('./inputs/18.txt')
input_coordinates = data.frame(
  x = as.numeric(split_vectorised(input_lines, ',', 1)),
  y = as.numeric(split_vectorised(input_lines, ',', 2)),
  z = as.numeric(split_vectorised(input_lines, ',', 3))
)

# Part 1
coordinates_copy_for_surfaces =
  input_coordinates %>%
  mutate(x_surface = x, y_surface = y, z_surface = z)
rbind(coordinates_copy_for_surfaces %>% mutate(x_surface = x_surface - 1),
      coordinates_copy_for_surfaces %>% mutate(x_surface = x_surface + 1),
      coordinates_copy_for_surfaces %>% mutate(y_surface = y_surface - 1),
      coordinates_copy_for_surfaces %>% mutate(y_surface = y_surface + 1),
      coordinates_copy_for_surfaces %>% mutate(z_surface = z_surface - 1),
      coordinates_copy_for_surfaces %>% mutate(z_surface = z_surface + 1)) %>%
  mutate(surface_name = paste0(x_surface, ',', y_surface, ',', z_surface)) %>%
  count(surface_name %in% input_lines)
#3448

# Part 2
# Could use the distance algorithm from day 12 or a graph based approach, 
# situation is small enough for either
summary(input_coordinates)
# coordinates are between 0 and 19
#Everything is accessible now using a grid between -1 and 20
all_coordinates = data.frame(
  x = rep(-1:20, times = 22 ^ 2),
  y = rep(-1:20, each = 22, times = 22),
  z = rep(-1:20, each = 22 ^ 2)
) %>%
mutate(x_surface = x, y_surface = y, z_surface = z)
graph_df = rbind(all_coordinates %>% mutate(x_surface = x_surface - 1),
      all_coordinates %>% mutate(x_surface = x_surface + 1),
      all_coordinates %>% mutate(y_surface = y_surface - 1),
      all_coordinates %>% mutate(y_surface = y_surface + 1),
      all_coordinates %>% mutate(z_surface = z_surface - 1),
      all_coordinates %>% mutate(z_surface = z_surface + 1)) %>%
  transmute(adjacent_point = paste0(x_surface, ',', y_surface, ',', z_surface),
         point = paste0(x, ',', y, ',', z))

adjacency_graph = graph_from_data_frame(graph_df, directed = FALSE)
adjacency_graph_without_main_nodes = subgraph(
  adjacency_graph, setdiff(graph_df$point, input_lines))


remaining_components = components(adjacency_graph_without_main_nodes)
external_nodes = names(remaining_components$membership[
  remaining_components$membership == 1
])
rbind(coordinates_copy_for_surfaces %>% mutate(x_surface = x_surface - 1),
      coordinates_copy_for_surfaces %>% mutate(x_surface = x_surface + 1),
      coordinates_copy_for_surfaces %>% mutate(y_surface = y_surface - 1),
      coordinates_copy_for_surfaces %>% mutate(y_surface = y_surface + 1),
      coordinates_copy_for_surfaces %>% mutate(z_surface = z_surface - 1),
      coordinates_copy_for_surfaces %>% mutate(z_surface = z_surface + 1)) %>%
  mutate(surface_name = paste0(x_surface, ',', y_surface, ',', z_surface)) %>%
  count(surface_name %in% input_lines, surface_name %in% external_nodes)
#2052