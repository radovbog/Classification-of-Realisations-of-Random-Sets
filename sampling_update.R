# Capture the command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Base directory containing the input data
base_dir <- args[1]
#base_dir <- "~/Desktop/ind/"
#base_dir<-"C:/Users/Spravce/Desktop"

# Number of runs
#n_runs <- args[2]
n_runs <- 50

# Number of realisations we consider
#n_reals <- args[3]
n_reals <- 200

# Realisations start indexing from 0
n_real <- n_reals - 1

# The size of the osculating radius
radius <- args[2]
#radius <- 5

#processes <- c(
#  paste0(base_dir,"/", radius, "/outputs_new/Boolean_matrix_200_", radius, "/out_", radius, "_"),
#  paste0(base_dir,"/", radius, "/outputs_new/Cluster_new_200_", radius, "/out_", radius, "_"),
#  paste0(base_dir,"/", radius, "/outputs_new/Repulsive_matrix_200_", radius, "/out_", radius, "_")
#)

# Considered processes
n_classes <- 3

# Loop through different runs and set the seed for each run to make your partition reproducible
for(n_run in 1:n_runs){
  set.seed(n_run)
  
  # Load data to a list and find samples for 10, 20 and all

  # X_list <- list()
  X_list <- readRDS(paste0(base_dir, "/X_list_", radius, ".rds"))
  samp_ind_list_10 <- list()
  samp_ind_list_20 <- list()
  samp_ind_list_all <- list()
  # for(I in 1:n_classes){
  #   name_prefix <- processes[I]
  #   counter_i <- 1
  #   for(i in 0:n_real){
  #     var_name <- paste("X", I, i, sep = "_")
  #      name <- paste0(name_prefix, i, ".txt")
  #     data <- read.table(name)
  #     data <- X_list[[var_name]]
  #     x1 <- dim(data)[1]
  #     n <- dim(data)[2]
  # 
  #     #Normalization
  #     for(j in 1:x1){
  #       data[j, 2:n] <- data[j, 2:n] / sum(data[, 2:n])
  #     }
  # 
  #     X_list[[var_name]] <- data
  
    
      for(I in 1:n_classes){
        for(i in 0:n_real){
          var_name<- paste("X", I, i, sep = "_")
          x1 <- dim(X_list[[var_name]])[1]
          rand_sample_10 <- sample(1:x1, 10)
          rand_sample_10_part <- sample(setdiff(1:x1, rand_sample_10), 10)
          rand_sample_20 <- rbind(rand_sample_10, rand_sample_10_part)
          
          samp_ind_list_10[[var_name]] <- rand_sample_10
          samp_ind_list_20[[var_name]] <- rand_sample_20
      
    
    } #i
  } #I

 # saveRDS(X_list, paste0("X_list_", radius, ".rds"))
  saveRDS(samp_ind_list_10, file =paste0(base_dir, "/", n_run, "_run_", n_reals, "_real_ind_list_10.rds"))
  saveRDS(samp_ind_list_20, file =paste0(base_dir, "/", n_run, "_run_", n_reals, "_real_ind_list_20.rds"))

#  Sampling for all components
   
#   samp_ind_list_20 <- readRDS(paste0(base_dir, n_run, "_run_", n_reals, "_real_ind_list_20.rds"))
  
  
  for(I in 1:n_classes){
    for(J in 1:n_classes){
      for(i in 0:n_real){
        var_name_I <- paste("X", I, i, sep = "_")
        X1 <- X_list[[var_name_I]]
        x1 <- dim(X1)[1]
        for(j in 0:n_real){
          var_name_J <- paste("X", J, j, sep = "_")
          X2 <- X_list[[var_name_J]]
          x2 <- dim(X2)[1]

          minim <- min(x1, x2)
          posit <- paste(I, i, J, j, sep = "_")
       #   posit_sym <- paste(J, j, I, i, sep = "_")

          if(x1!=x2){
            if(minim==x1){
              rand_sample_all <- sample(setdiff(1:x2, samp_ind_list_20[[var_name_J]]), x1-20)
              samp_ind_list_all[[posit]]<- rand_sample_all
         #     samp_ind_list_all[[posit_sym]]<- rand_sample_all
            }
            else{
              rand_sample_all <- sample(setdiff(1:x1, samp_ind_list_20[[var_name_I]]), x2-20)
              samp_ind_list_all[[posit]]<- rand_sample_all
          #    samp_ind_list_all[[posit_sym]]<- rand_sample_all
            }
          }
          else{
            rand_sample_all<- 1:x1
            samp_ind_list_all[[posit]]<- rand_sample_all
         #   samp_ind_list_all[[posit_sym]]<- rand_sample_all
          }
        } #j
      } #i
    } #J
  } #I

  saveRDS(samp_ind_list_all, file =paste0(base_dir, "/", n_run, "_run_", n_reals, "_real_ind_list_All.rds"))
}


