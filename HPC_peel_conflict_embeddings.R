
###########################
###########################
# # 
# # MYRIAD RUN SCRIPT
# # 
### This script creates the embeddings for the facebook 100 dataset. It only uses auto_setse not setse_bicomp. it is for reference only
# # 
# # 
###########################
###########################

packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "magrittr",
              "igraph", "devtools", "minpack.lm", "readr", "stringr",
              "Matrix")


new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(packages, library, character.only = TRUE)

#Set up file system to read the correct folders this switches between aws and windows mode

#creates the correct root depending on whether this is on the cloud or not
if(dir.exists("/home/jonno")){
  #This folder is for use on my machine
  project_folder <- "/home/jonno/setse_1_data"
  basewd <- "/home/jonno"
  load_data_files_path <- file.path(project_folder) #load the files
  save_data_files_path <- file.path(jnjkhkj) #save the files this will cause an error as I have no path
  library(rSETSe)
  list.files("/home/jonno/Useful_PhD__R_Functions", pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
}else{
  #This is for the folder that is on the cloud
  project_folder <- getwd()
  
  basewd <- "/home/ucabbou"
  #on the home dir not in the project folder like when it is done on my own comp
  save_data_files_path <- file.path(project_folder) #save the files

  #If it is not on my computer then the variables need to be loaded from the system environment
  #Get the task ID
  task_id <- as.integer(Sys.getenv("SGE_TASK_ID"))

  list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
  list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
    walk(~source(.x))
  
}

#get the total job set
#only the row corresponding to the rask ID will be performed
simulation_combinations <- expand.grid(LETTERS[1:5], c("k_uniform", "k_varies"), stringsAsFactors = FALSE)

k_type <- simulation_combinations[job_id , 2]

graph_type <- simulation_combinations[job_id,1]

file_name <- paste0("peel_conflict_", k_type, "_graph_",graph_type, ".rds" )

if(k_type =="k_uniform"){
  
  k_levels <- c(100,100,100)
  
} else {
  
  k_levels <- c(100, 550, 1000)
}

#THIS SHOULD BE SET TO 40!!!!!!
combinations <- t(combn(1:3, 2))

peels_results <- peel_conflicts(graph_type = graph_type,
                                beligerents = tibble(node1 = combinations[,1], 
                                                     node2 = combinations[,2]), #nodes to test
                                k_levels = k_levels,
                                samples = 1) #SAMPLES SHOULD NOT BE 1!!!!

  
  saveRDS(peels_results, file = file_name )