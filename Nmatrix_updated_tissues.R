#library(readr)
#library(tidyverse)

##############################################################################
### N DISTANCE CALCULATION ###################################################
##############################################################################

zero_id <- function(X_list){
  M <- c()
 # for(i in 1:3){ # This for simulated
  for(i in 1:2){ # This for tissues
    for(j in 0:199){
      var_name <- paste("X", i, j, sep = "_")
      A <- X_list[[var_name]]
      M <- rbind(M, A)
    }
  }
  t<-colSums(M)
  id <- which(t==0)
  return(id)
}

N.dist.kernel <- function(a, b){
  return(sqrt(sum((a - b)^2)))
}

N.dist <- function(L, m1, m2) {
  N1 <- sum(L[(m1+1):(m1+m2), 1:m1])
  N2 <- sum(L[1:m1, 1:m1])
  N3 <- sum(L[(m1+1):(m1+m2), (m1+1):(m1+m2)])
  
  return(abs(2*N1 / (m1*m2) - N2 / m1^2 - N3 / m2^2))
}

semimetric.N.dist <- function(D1, D2) {
  M1 <- as.matrix(D1)
  M2 <- as.matrix(D2)
  # Get dimensions
  m1 <- nrow(M1)
  m2 <- nrow(M2)
  m <- m1 + m2
  M <- rbind(M1, M2)
  
  # Efficiently compute the kernel matrix L using outer function
  L <- outer(1:m, 1:m, Vectorize(function(i, j) N.dist.kernel(M[i, ], M[j, ])))
  # L=matrix(rep(0,m^2),nrow=m)
  # for(i in 1:m){
  #   for(j in 1:m){
  #     L[i,j]=N.dist.kernel(M[i,],M[j,])
  #   }
  # }
  
  # Calculate the semimetric value
  SEMIMETRIC <- N.dist(L, m1, m2)
  
  return(SEMIMETRIC)
}


# ####################################################
# ###################### MAIN ########################
# ####################################################

# Capture the command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Base directory containing the input data
base_dir <- args[1]
#base_dir <- "~/Desktop/"
#base_dir<-"C:/Users/Spravce/Desktop"

# The size of the osculating radius
radius <- args[2]
#radius <- 5

# Number of runs
#n_runs <- args[2]
n_runs <- 50
partial_run <- args[3]
#partial_run <- 1

# Number of realisations we consider
#n_reals <- args[4]
n_reals <- 200

# Realisations start indexing from 0
n_real <- n_reals - 1
#n_real <-2

# Considered processes
# n_classes <- 3
# names <- c("Boolean", "Cluster", "Repulsive")
# classes <- c("B", "C", "R")

# Considered tissues
n_classes <- 2
names <- c("Mammary cancer", "Mastopathy")
classes <- c("MC", "MP")

# Considered sample sizes
ss_names <- c("10", "20", "All") #, "All_perm")
  
# Load data 
X_list <- readRDS(paste0(base_dir, "/X_list_", radius, ".rds"))
zero_ids <- zero_id(X_list)
# n_run <-1
# Loop through different runs and set the seed for each run
#for(n_run in 1:n_runs){
for(n_run in partial_run:partial_run){

  set.seed(n_run)
  smp_20 <- readRDS(paste0(base_dir, "/ind/", n_run, "_run_", n_reals, "_real_ind_list_20.rds"))
  for(ss in 1:3){
#  ss<- 4 # THIS SHOULD BE COMMENTED AND THE LINE ABOVVE UNCOMMENTED
    sample_size <- ss_names[ss]
    sample_ind_list <- readRDS(paste0(base_dir, "/ind/", n_run, "_run_", n_reals, "_real_ind_list_", sample_size, ".rds"))
    
    N.dist.matrix_B <- matrix(0,nrow=n_classes*n_reals,ncol=n_classes*n_reals)
    N.dist.matrix_R <- matrix(0,nrow=n_classes*n_reals,ncol=n_classes*n_reals)
    N.dist.matrix_C <- matrix(0,nrow=n_classes*n_reals,ncol=n_classes*n_reals)

    for(I in 1:n_classes){
      for(J in I:n_classes){
        for(i in 0:n_real){
          idx_i = (I-1)*n_reals+i+1
          
          # Realisation X1
          var_name_I <- paste("X", I, i, sep = "_")
          X1 <- as.matrix(X_list[[var_name_I]])
          X1 <- X1[, -zero_ids]
          n1 <- dim(X1)[2]
          
          if(ss == 3 || ss==4){
            x1 <- dim(X1)[1]
          }
          else{
            X1S <- X1[sample_ind_list[[var_name_I]],]
            X1R <- X1S[, 1]
            X1C <- X1S[, 2:n1]
          }
          
          for(j in 0:n_real){
            idx_j = (J-1)*n_reals+j+1
            
            # Realisation X2
            var_name_J <- paste("X", J, j, sep = "_")
            X2 <- as.matrix(X_list[[var_name_J]])
            X2 <- X2[, -zero_ids]
            x2 <- dim(X2)[1]
            n2 <- dim(X2)[2]
            

            if(ss == 3){
              pos <- paste(I, i, J, j, sep="_")
              
              # If X1 has more components than X2
              if(x1>x2){
                smp <- c(smp_20[[var_name_I]], sample_ind_list[[pos]])
                X1S <- X1[smp,]
                X2S <- X2
              }
              # If X1 has less components than X2
              else if(x2>x1){
                smp <- c(smp_20[[var_name_J]], sample_ind_list[[pos]])
                X2S <- X2[smp,]
                X1S <- X1
              }
              else{
                X1S <- X1
                X2S <- X2
              }
              X1R <- X1S[,1]
              X1C <- X1S[, 2:n1]
              X2R <- X2S[,1]
              X2C <- X2S[, 2:n2]
            } else if(ss == 4){
              smp_1 <- sample_ind_list[[var_name_I]]
              smp_2 <- sample_ind_list[[var_name_J]]
              
              # If X1 has more components than X2
              if(x1>x2){
                smp <- smp_1[1:x2]
                X1S <- X1[smp,]
                X2S <- X2[smp_2,]
              }
              # If X1 has less or equal components than X2
              else{
                smp <- smp_2[1:x1]
                X2S <- X2[smp,]
                X1S <- X1[smp_1,]
              }

              X1R <- X1S[,1]
              X1C <- X1S[, 2:n1]
              X2R <- X2S[,1]
              X2C <- X2S[, 2:n2]
              
            }
            else{
              X2S <- X2[sample_ind_list[[var_name_J]],]
              X2R <- X2S[, 1]
              X2C <- X2S[, 2:n2]
            }
            
            X1R <- matrix(X1R, ncol=1)
            X2R <- matrix(X2R, ncol=1)
            
            
            # Filling the N-distance matrices 
            SEMIMETRIC_B = semimetric.N.dist(as.matrix(X1S),as.matrix(X2S))
            N.dist.matrix_B[idx_i, idx_j]=SEMIMETRIC_B
            N.dist.matrix_B[idx_j, idx_i]=SEMIMETRIC_B

            SEMIMETRIC_R <- semimetric.N.dist(X1R, X2R) #as.matrix(M1R),as.matrix(M2R))
            N.dist.matrix_R[idx_i, idx_j] <- SEMIMETRIC_R
            N.dist.matrix_R[idx_j, idx_i] <- SEMIMETRIC_R
            

            SEMIMETRIC_C <- semimetric.N.dist(X1C,X2C)
            N.dist.matrix_C[idx_i, idx_j] <- SEMIMETRIC_C
            N.dist.matrix_C[idx_j, idx_i] <- SEMIMETRIC_C
            
            
            # Ovaj korak na kraju kao funckiju u zavisnosti na a
            # N.dist.matrix_train_a_10[(I-1)*length(train_ind_B)+counter_i,(J-1)*length(train_ind_B)+counter_j]= a*SEMIMETRIC_R_10 + (1-a)*SEMIMETRIC_C_10
          } #j
        } #i
      } #J
    } #I
    
    saveRDS(N.dist.matrix_R, file =paste0(n_run, "_run_Ndist_",n_reals, "_real_r_", radius, "_R_", sample_size ,".rds"))
    saveRDS(N.dist.matrix_C, file =paste0(n_run, "_run_Ndist_",n_reals, "_real_r_", radius, "_C_", sample_size ,".rds"))
    saveRDS(N.dist.matrix_B, file =paste0(n_run, "_run_Ndist_",n_reals, "_real_r_", radius, "_B_", sample_size ,".rds"))
    
    write.table(N.dist.matrix_R, file =paste0(n_run, "_run_Ndist_",n_reals, "_real_r_", radius, "_R_", sample_size ,".txt"), row.names = FALSE, col.names = FALSE)
    write.table(N.dist.matrix_C, file =paste0(n_run, "_run_Ndist_",n_reals, "_real_r_", radius, "_C_", sample_size ,".txt"), row.names = FALSE, col.names = FALSE)
    write.table(N.dist.matrix_B, file =paste0(n_run, "_run_Ndist_",n_reals, "_real_r_", radius, "_B_", sample_size ,".txt"), row.names = FALSE, col.names = FALSE)
  }   ## ss loop
    
} #skracen program
