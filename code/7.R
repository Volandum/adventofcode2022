input_lines = readLines('./inputs/7.txt')

# Each directory (except '/') has a parent
# Directories - name, parent
# Each file has name, parent directory and size
# Files - name, parent directory, size

directories = data.frame(name = '/', parent = NA_character_,
                         path = '/')
files = data.frame(name = rep(NA_character_, 0), 
                   path = rep(NA_character_, 0), 
                   size = rep(NA_integer_, 0))
current_path = '/'
for(line in input_lines){
  if(line == '$ ls'){
    # do nothing
  } else if (line == '$ cd ..'){
    current_path = gsub('\\w+/$', '', current_path)
  }
  else if (grepl('\\$ cd', line)){
    # change current directory
    new_directory = gsub('^\\$ cd ', '', line)
    if(new_directory == '/'){
      current_path = '/'
    } else {
      current_path = paste0(current_path, new_directory, '/')
    }
  } else {
    if (grepl('^dir', line)){
      new_directory = gsub('^dir ', '', line)
      directories = rbind(directories, data.frame(name = new_directory, 
                                                  parent = current_path,
                                                  path = paste0(current_path, new_directory, '/')))
      # may create duplicates
    } else {
      filename = gsub('\\d+ ', '', line)
      size = as.numeric(gsub('(\\d+) .+', '\\1', line))
      files = rbind(files, 
                    data.frame(name = filename, path = current_path, size = size))
    }
  }
}

unique_directories = unique(directories)
unique_files = unique(files)

path_base_sizes = files %>% group_by(path) %>% summarise(base_size = sum(size))
path_total_sizes = sqldf('select path1.path, sum(path2.base_size) as total_size
      from directories path1 inner join path_base_sizes path2 on
      instr(path2.path, path1.path)  = 1
      group by path1.path')

path_total_sizes %>% filter(total_size <= 100000) %>% pull(total_size) %>% sum
# 2031851
# Beware as not all directories contain files directly!

total_file_size = sum(files$size)
space_needed = 30000000 - (70000000 - total_file_size)
path_total_sizes %>% filter(total_size >= space_needed) %>% arrange(total_size)
# 2568781