input_lines = readLines('./inputs/19.txt')
input = data.frame(
  id = unname(get_regex_match(input_lines, 'Blueprint (\\d+):', 1)),
  ore_cost = unname(get_regex_match(input_lines, 'Each ore robot costs ([ \\w\\d]+)\\.', 1)),
  clay_cost = unname(get_regex_match(input_lines, 'Each clay robot costs ([ \\w\\d]+)\\.', 1)),
  obsidian_cost = unname(get_regex_match(input_lines, 'Each obsidian robot costs ([ \\w\\d]+)\\.', 1)),
  geode_cost = unname(get_regex_match(input_lines, 'Each geode robot costs ([ \\w\\d]+)\\.', 1))
)

#geode always costs ore + obsidian, obsidian always costs ore + clay, ore always costs ore

input = input %>%
  mutate(ore_ore_cost = unname(get_regex_match(ore_cost, '(\\d+) ore')),
         clay_ore_cost = unname(get_regex_match(clay_cost, '(\\d+) ore')),
         obsidian_ore_cost = unname(get_regex_match(obsidian_cost, '(\\d+) ore')),
         obsidian_clay_cost = unname(get_regex_match(obsidian_cost, '(\\d+) clay')),
         geode_ore_cost = unname(get_regex_match(geode_cost, '(\\d+) ore')),
         geode_obsidian_cost = unname(get_regex_match(geode_cost, '(\\d+) obsidian')))

# Linear programming?
library(lpSolve)

solve_blueprint = function(ore_ore_cost, clay_ore_cost,  obsidian_ore_cost, obsidian_clay_cost, geode_ore_cost, geode_obsidian_cost,
                           starting_ore_robots = 1, turns = 24){
  # Our variables are 12 * turns + 1 vectors, representing:
  # 1. Ore bots being built
  # 2. Ore bots
  # 3. Ore at start of turn after paying for bots
  # 4. Clay bots being built/ 5. Clay bots / 6. Clay at start of turn after paying for bots
  # 7. Obsidian bots being built / 8. Obsidian bots / 9. Obsidian at start of turn after paying for bots /
  # 10. Geode bots being built / 11. Geode bots / 12. Geodes at start of turn (vector of length + 1)
  # Test case
  # ore_ore_cost = 2
  # clay_ore_cost = 3
  # obsidian_ore_cost = 3
  # obsidian_clay_cost = 8
  # geode_ore_cost = 3
  # geode_obsidian_cost = 12
  # turns = 24
  # starting_ore_robots = 1
  lp_objective = c(rep(0, 12 * turns),1)
  # Starting bot counts
  bot_count_constr = rbind(
    matrix(c(rep(0, turns), 1, rep(0, 11 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 4), 1, rep(0, 8 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 7), 1, rep(0, 5 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 10), 1, rep(0, 2 * turns)), nrow = 1, byrow = T)
  )
  bot_count_rhs = c(starting_ore_robots, 0,0,0)
  bot_count_dir = c('==', '==', '==', '==')
  
  # Turn 1 bot construction
  bot_initial_build_constr = rbind(
    matrix(c(1, rep(0, 12 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 3), 1, rep(0, 9 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 6), 1, rep(0, 6 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 9), 1, rep(0, 3 * turns)), nrow = 1, byrow = T)
  )
  bot_initial_build_rhs = c(0, 0,0,0)
  bot_initial_build_dir = c('==', '==', '==', '==')
  
  
  # Starting resources
  resource_count_constr = rbind(
    matrix(c(rep(0, turns * 2), 1, rep(0, 10 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 5), 1, rep(0, 7 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 8), 1, rep(0, 4 * turns)), nrow = 1, byrow = T),
    matrix(c(rep(0, turns * 11), 1, rep(0, 1 * turns)), nrow = 1, byrow = T)
  )
  resource_count_rhs = c(0,0,0,0)
  resource_count_dir = c('==', '==', '==', '==')
  
  # Bot updates
  ore_bot_update_constr = t(sapply(
    2:turns,
    function(turn){
      c(rep(0, turn - 2),
        1,#bot being built on prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        1,#previous turn bot count
        -1, #current turn bot count
        rep(0, turns - turn),
        rep(0, 10*turns + 1))
    }
  ))
  ore_bot_update_rhs = rep(0, turns - 1)
  ore_bot_update_dir = rep('==', turns - 1)
  clay_bot_update_constr = t(sapply(
    2:turns,
    function(turn){
      c(rep(0, 3 * turns),
        rep(0, turn - 2),
        1,#bot being built on prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        1,#previous turn bot count
        -1, #current turn bot count
        rep(0, turns - turn),
        rep(0, 7*turns + 1))
    }
  ))
  clay_bot_update_rhs = rep(0, turns - 1)
  clay_bot_update_dir = rep('==', turns - 1)
  obsidian_bot_update_constr = t(sapply(
    2:turns,
    function(turn){
      c(rep(0, 6 * turns),
        rep(0, turn - 2),
        1,#bot being built on prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        1,#previous turn bot count
        -1, #current turn bot count
        rep(0, turns - turn),
        rep(0, 4*turns + 1))
    }
  ))
  obsidian_bot_update_rhs = rep(0, turns - 1)
  obsidian_bot_update_dir = rep('==', turns - 1)
  geode_bot_update_constr = t(sapply(
    2:turns,
    function(turn){
      c(rep(0, 9 * turns),
        rep(0, turn - 2),
        1,#bot being built on prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        1,#previous turn bot count
        -1, #current turn bot count
        rep(0, turns - turn),
        rep(0, 1*turns + 1))
    }
  ))
  geode_bot_update_rhs = rep(0, turns - 1)
  geode_bot_update_dir = rep('==', turns - 1)
  
  # Resource updates
  ore_update_constr = t(sapply(
    2:turns,
    function(turn){
      c(rep(0, turn - 1),
        ore_ore_cost,#bot being built on current turn
        rep(0, turns - turn),
        rep(0, turn - 2), 
        -1,#ore bots active prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        -1,#previous turn ore count
        1, #current turn ore count
        rep(0, turns - turn),
        rep(0, turn - 1),
        clay_ore_cost,#bot being built on current turn
        rep(0, turns - turn),
        rep(0, turns * 2),
        rep(0, turn - 1),
        obsidian_ore_cost,#bot being built on current turn
        rep(0, turns - turn),
        rep(0, turns * 2),
        rep(0, turn - 1),
        geode_ore_cost,#bot being built on current turn
        rep(0, turns - turn),
        rep(0, turns * 2 + 1)
      )
    }
  ))
  ore_update_rhs = rep(0, turns - 1)
  ore_update_dir = rep('==', turns - 1)
  
  clay_update_constr = t(sapply(
    2:turns,
    function(turn){
      c(rep(0, turns * 4),
        rep(0, turn - 2), 
        -1,#clay bots active prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        -1,#previous turn clay count
        1, #current turn clay count
        rep(0, turns - turn),
        rep(0, turn - 1),
        obsidian_clay_cost,#bot being built on current turn
        rep(0, turns - turn),
        rep(0, turns * 5 + 1)
      )
    }
  ))
  clay_update_rhs = rep(0, turns - 1)
  clay_update_dir = rep('==', turns - 1)
  
  obsidian_update_constr = t(sapply(
    2:turns,
    function(turn){
      c(rep(0, turns * 7),
        rep(0, turn - 2), 
        -1,#obsidian bots active prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        -1,#previous turn obsidian count
        1, #current turn obsidian count
        rep(0, turns - turn),
        rep(0, turn - 1),
        geode_obsidian_cost,#bot being built on current turn
        rep(0, turns - turn),
        rep(0, turns * 2 + 1)
      )
    }
  ))
  obsidian_update_rhs = rep(0, turns - 1)
  obsidian_update_dir = rep('==', turns - 1)
  
  geode_update_constr = t(sapply(
    2:(turns + 1),
    function(turn){
      c(rep(0, turns * 10),
        rep(0, turn - 2), 
        -1,#clay bots active prev turn
        rep(0, turns - turn + 1),
        rep(0, turn - 2),
        -1,#previous turn clay count
        1, #current turn clay count
        rep(0, turns + 1 - turn)
      )
    }
  ))
  geode_update_rhs = rep(0, turns)
  geode_update_dir = rep('==', turns)
  
  # Build one robot at a time
  
  robot_build_constr = t(sapply(
    1:turns,
    function(turn){
      c(rep(0, turn - 1),
        1, #ore bots being built
        rep(0, turns - turn),
        rep(0, turns * 2),
        rep(0, turn - 1),
        1, #clay bots being built
        rep(0, turns - turn),
        rep(0, turns * 2),
        rep(0, turn - 1),
        1, #obsidian bots being built
        rep(0, turns - turn),
        rep(0, turns * 2),
        rep(0, turn - 1),
        1, #geode bots being built
        rep(0, turns - turn),
        rep(0, turns * 2 + 1)
      )
    }
  ))
  robot_build_rhs = rep(1, turns)
  robot_build_dir = rep('<=', turns)
  
  lp_constraints = rbind(
    bot_count_constr, 
    bot_initial_build_constr,
    resource_count_constr, 
    ore_bot_update_constr, 
    clay_bot_update_constr, 
    obsidian_bot_update_constr, 
    geode_bot_update_constr, 
    ore_update_constr, 
    clay_update_constr, 
    obsidian_update_constr, 
    geode_update_constr, 
    robot_build_constr
  )
  
  lp_rhs = c(
    bot_count_rhs, 
    bot_initial_build_rhs,
    resource_count_rhs, 
    ore_bot_update_rhs, 
    clay_bot_update_rhs, 
    obsidian_bot_update_rhs, 
    geode_bot_update_rhs, 
    ore_update_rhs, 
    clay_update_rhs, 
    obsidian_update_rhs, 
    geode_update_rhs, 
    robot_build_rhs
  )
  
  lp_dir = c(
    bot_count_dir, 
    bot_initial_build_dir,
    resource_count_dir, 
    ore_bot_update_dir, 
    clay_bot_update_dir, 
    obsidian_bot_update_dir, 
    geode_bot_update_dir, 
    ore_update_dir, 
    clay_update_dir, 
    obsidian_update_dir, 
    geode_update_dir, 
    robot_build_dir
  )
  
  lp_object = lp ("max", lp_objective, lp_constraints, lp_dir, lp_rhs, 
          binary.vec = c(1:turns, turns*3 + 1:turns, turns * 6 + 1:turns, turns * 9 + 1:turns))
  return(lp_object)
}
u = solve_blueprint(4, 2, 3, 14, 2, 7)
v = solve_blueprint(2, 3, 3, 8, 3, 12)
solved_lp_objects = list()
for(row in 1:nrow(input)){
  message(row)
  solved_lp_objects[[row]] = 
    solve_blueprint(
      ore_ore_cost = input$ore_ore_cost[row],
      clay_ore_cost = input$clay_ore_cost[row],
      obsidian_ore_cost = input$obsidian_ore_cost[row],
      obsidian_clay_cost = input$obsidian_clay_cost[row],
      geode_ore_cost = input$geode_ore_cost[row],
      geode_obsidian_cost = input$geode_obsidian_cost[row]
    )
} # ran overnight

save(file = 'day19lpobjects.RData', list = 'solved_lp_objects')

optimal_values = sapply(1:30,
                        function(n){
                          solved_lp_objects[[n]]$objval
                        })

sum(1:30*optimal_values)
#1962

# Does optimisation work for 32 turns?

solved_lp_objects_32_turns = list()
for(row in 1:3){
  message(row)
  message(Sys.time())
  solved_lp_objects_32_turns[[row]] = 
    solve_blueprint(
      ore_ore_cost = input$ore_ore_cost[row],
      clay_ore_cost = input$clay_ore_cost[row],
      obsidian_ore_cost = input$obsidian_ore_cost[row],
      obsidian_clay_cost = input$obsidian_clay_cost[row],
      geode_ore_cost = input$geode_ore_cost[row],
      geode_obsidian_cost = input$geode_obsidian_cost[row],
      turns = 32
    )
} #strangely much much faster than the 24 turns run

save(file = 'day19lpobjectspt2.RData', list = 'solved_lp_objects_32_turns')

prod(sapply(1:3,
       function(n){
         solved_lp_objects_32_turns[[n]]$objval
       }))
