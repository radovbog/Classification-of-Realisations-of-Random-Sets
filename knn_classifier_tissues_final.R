library(ggplot2)
library(readr)
library(tidyverse)


# This part of the code comes from:
# https://www.math.univ-toulouse.fr/~ferraty/SOFTWARES/NPFDA/npfda-content.html
# (provided as supplementary material to the book "NonParametric Functional Data Analysis")
# (by F. Ferraty and P. Vieu)

####################################################################
triangle <- function(u)
{
  #  triangle kernel
  1 - u
}
#####################################################################
quadratic <- function(u)
{
  #  quadratic kernel
  1 - (u)^2
}
####################################
### ASYMMETRICAL QUADRATIC KERNEL  #
####################################
quadratic2 <- function(u) 
{
  u[u<0] <- 1
  u[u>1] <- 1 
  return(1 - (u)^2) 
}
#####################################################################

funopadi.knn.lcv <- function(Classes, SEMIMETRIC1, SEMIMETRIC2, kind.of.kernel = "quadratic", twodatasets = 1)
{
  ################################################################
  # Performs functional discrimination of a sample of curves when 
  # a categorical response is observed (supervised classification). 
  # A local bandwidth (i.e. local number of neighbours) is selected 
  # by a cross-validation procedure.
  #    "Classes" vector containing the categorical responses
  #              giving the group number for each curve in 
  #              the matrix CURVES (if nbclass is the number of 
  #              groups, "Classes" contains numbers 1,2,...,nbclass)
  #    "CURVES" matrix containing the curves dataset (row by row) 
  #             used for the estimating stage
  #    "PRED" matrix containing new curves stored row by row
  #           used for computing predictions
  #    "..." arguments needed for the call of the function computing 
  #          the semi-metric between curves
  #    "kind.of.kernel" the kernel function used for computing of 
  #                     the kernel estimator; you can choose 
  #                     "indicator", "triangle" or "quadratic (default)
  #    "semimetric" character string allowing to choose the function 
  #                 computing the semimetric;  you can select 
  #                 "deriv" (default), "fourier", "hshift", "mplsr", 
  #                 and "pca"
  # Returns a list containing:
  #    "Estimated.classnumber" vector containing estimated class membership 
  #                            for each curve of "CURVES"
  #    "Predicted.classnumber" if PRED different from CURVES, this vector 
  #                            contains predicted class membership for each 
  #                            curve of PRED
  #    "Bandwidths" vector containing the local data-driven bandwidths
  #                 for each curve in the matrix "CURVES"
  #    "Misclas" misclassification rate computed from estimated values and 
  #              observed values
  ################################################################
  Classes <- as.vector(Classes)
  kernel <- get(kind.of.kernel)
  SEMIMETRIC1 <- as.matrix(SEMIMETRIC1)
  SEMIMETRIC2 <- as.matrix(SEMIMETRIC2)
  n1 <- ncol(SEMIMETRIC1)
  step <- ceiling(n1/100)
  if(step == 0)
    step <- 1
  Knearest <- seq(from = 10, to = n1 %/% 2, by = step)
  kmax <- max(Knearest)
  # the vector Knearest contains the sequence of the 
  # k-nearest neighbours used for computing the optimal bandwidth
  Classes.estimated <- 0
  Bandwidth.opt <- 0
  nbclass <- max(Classes)
  BINARY <- matrix(0, n1, nbclass)
  for(g in 1:nbclass)
    BINARY[, g] <- as.numeric(Classes == g)
  HAT.PROB <- matrix(0, nrow = nbclass, ncol = length(Knearest))
  Knn1 <- 0
  for(i in 1:n1) {
    Norm.diff <- SEMIMETRIC1[, i]
    # "norm.order" gives the sequence k_1, k_2,... such that
    # dq(X_{k_1},X_i) < dq(X_{k_2},X_i) < ...
    Norm.order <- order(Norm.diff)
    # "zz" contains dq(X_{k_2},X_i), dq(X_{k_3},X_i),..., 
    # dq(X_{j_{kamx+2}},X_i)
    zz <- sort(Norm.diff)[2:(kmax + 2)]
    # Bandwidth[l-1] contains (dq(X_{j_l},X_i) + 
    # dq(X_{j_l},X_i))/2 for l=2,...,kmax+2
    Bandwidth <- 0.5 * (zz[-1] + zz[ - (kmax + 1)])
    z <- zz[ - (kmax + 1)]
    ZMAT <- matrix(rep(z, kmax), nrow = kmax, byrow = T)
    UMAT <- ZMAT/Bandwidth
    KMAT <- kernel(UMAT)
    KMAT[col(KMAT) > row(KMAT)] <- 0
    Ind.curves <- Norm.order[2:(kmax + 1)]
    for(g in 1:nbclass) {
      Ind.resp <- BINARY[Ind.curves, g]
      YMAT <- matrix(rep(Ind.resp, kmax), nrow = kmax, byrow
                     = T)
      HAT.PROB[g,  ] <- apply(YMAT[Knearest,  ] * KMAT[
        Knearest,  ], 1, sum)
    }
    Kmatsumbyrow <- apply(KMAT[Knearest,  ], 1, sum)
    HAT.PROB <- HAT.PROB/matrix(Kmatsumbyrow,nrow(HAT.PROB), ncol(HAT.PROB), byrow=T)
    Criterium <- t(rep(1, nbclass)) %*% (HAT.PROB - BINARY[i,  ])^2
    index <- order(as.vector(Criterium))[1]
    Knn1[i] <- Knearest[index]
    Classes.estimated[i] <- order(HAT.PROB[, index])[nbclass]
    Bandwidth.opt[i] <- Bandwidth[index]
  }
  Misclas.estimated <- sum(Classes.estimated != Classes)/n1
  if(twodatasets) {
    Bandwidth2 <- 0
    n2 <- ncol(SEMIMETRIC2)
    for(k in 1:n2) {
      Sm2k <- SEMIMETRIC2[, k]
      Sm2k.ord <- order(SEMIMETRIC2[, k])
      knn <- Knn1[Sm2k.ord[1]]
      Bandwidth2[k] <- sum(sort(Sm2k)[knn:(knn+1)])*0.5
    }
    KERNEL <- kernel(t(t(SEMIMETRIC2)/Bandwidth2))
    KERNEL[KERNEL < 0] <- 0
    KERNEL[KERNEL > 1] <- 0
    Denom <- apply(as.matrix(KERNEL), 2, sum)
    PROB.PREDICTED <- matrix(0, nrow = n2, ncol = nbclass)
    for(g in 1:nbclass) {
      PROBKERNEL <- KERNEL * BINARY[, g]
      PROB.PREDICTED[, g] <- apply(as.matrix(PROBKERNEL), 2, 
                                   sum)/Denom
    }
    Classes.predicted <- as.vector((PROB.PREDICTED == apply(
      PROB.PREDICTED, 1, max)) %*% (1:nbclass))
    return(list(Estimated.classnumber = Classes.estimated, 
                Predicted.classnumber = Classes.predicted, Bandwidths
                = Bandwidth.opt, Misclas = Misclas.estimated))
  }else {
    return(list(Estimated.classnumber = Classes.estimated, 
                Bandwidths = Bandwidth.opt, Misclas = Misclas.estimated))
  }
}

##############################################################################
######################### ADDED BY BOGDAN RADOVIC#############################
##############################################################################
# Parts of this code come from Vesna Gotovac from University of Split
# https://github.com/VesnaGotovac?tab=repositories


# misclassification_stats <- function(y_true, y_pred) {
#   # Calculate and print misclassifications on the test set
#   classified_B_as_B <- sum((y_true == 1) & (y_pred == 1))
#   misclassified_B_as_C <- sum((y_true == 1) & (y_pred == 2))
#   misclassified_B_as_R <- sum((y_true == 1) & (y_pred == 3))
#   misclassified_C_as_B <- sum((y_true == 2) & (y_pred == 1))
#   classified_C_as_C <- sum((y_true == 2) & (y_pred == 2))
#   misclassified_C_as_R <- sum((y_true == 2) & (y_pred == 3))
#   misclassified_R_as_B <- sum((y_true == 3) & (y_pred == 1))
#   misclassified_R_as_C <- sum((y_true == 3) & (y_pred == 2))
#   classified_R_as_R <- sum((y_true == 3) & (y_pred == 3))
#   
#   # Return the calculated misclassifications
#   return(list(
#     classified_B_as_B = classified_B_as_B,
#     misclassified_B_as_C = misclassified_B_as_C,
#     misclassified_B_as_R = misclassified_B_as_R,
#     misclassified_C_as_B = misclassified_C_as_B,
#     classified_C_as_C = classified_C_as_C,
#     misclassified_C_as_R = misclassified_C_as_R,
#     misclassified_R_as_B = misclassified_R_as_B,
#     misclassified_R_as_C = misclassified_R_as_C,
#     classified_R_as_R = classified_R_as_R
#   ))
# }

misclassification_stats <- function(y_true, y_pred) {
  # Calculate and print misclassifications on the test set
  classified_MC_as_MC <- sum((y_true == 1) & (y_pred == 1))
  misclassified_MC_as_MP <- sum((y_true == 1) & (y_pred == 2))
  misclassified_MP_as_MC <- sum((y_true == 2) & (y_pred == 1))
  classified_MP_as_MP <- sum((y_true == 2) & (y_pred == 2))
  
  # Return the calculated misclassifications
  return(list(
    classified_MC_as_MC = classified_MC_as_MC,
    misclassified_MC_as_MP = misclassified_MC_as_MP,
    misclassified_MP_as_MC = misclassified_MP_as_MC,
    classified_MP_as_MP = classified_MP_as_MP
  ))
}

# train_test_divide <- function(num_classes, num_reals, train_size){
#   # "num_classes" - number of different classes (processes) that are considered
#   # "num_reals" - number of realisations we consider
#   # "percent_train" - train test size in percents 
#   
#   # Define indexing
#   index_all = 1:num_reals
#   
#   # Define array for storing training and testing indices
#   train_ind <- c()
#   test_ind <- c()
#   
#   # Define array for storing training and testing labels
#   train_lbl <- c()
#   test_lbl <- c()
#   
#   for(i in 1:num_classes){
#     temp_train <- sample(index_all, size = train_size)
#     train_ind <- c(train_ind, (i-1)*200+temp_train)
#     temp_test <- index_all[!(index_all %in% temp_train)]
#     test_ind <- c(test_ind, (i-1)*200+temp_test)
#     train_lbl <- c(train_lbl, rep(i, train_size))
#     test_lbl <- c(test_lbl, rep(i, num_real - train_size))
#   }
# 
#   return(list(train = train_ind, 
#               test = test_ind, 
#               train_lbl = train_lbl, 
#               test_lbl = test_lbl)
#               )
#   
# }


train_test_divide <- function(num_classes, num_reals, train_size){
  # "num_classes" - number of different classes (processes) that are considered
  # "num_reals" - number of realisations we consider
  # "percent_train" - train test size in percents 
  
  # Define indexing
  index_all = 1:num_reals
  
  # Define array for storing training and testing indices
  train_ind <- c()
  test_ind <- c()
  
  # Define array for storing training and testing labels
  train_lbl <- c()
  test_lbl <- c()
  
  for(i in 1:num_classes){
    temp_train <- sample(index_all, size = train_size)
    train_ind <- c(train_ind, (i-1)*num_real+temp_train)
    temp_test <- index_all[!(index_all %in% temp_train)]
    test_ind <- c(test_ind, (i-1)*num_real+temp_test)
    train_lbl <- c(train_lbl, rep(i, train_size))
    test_lbl <- c(test_lbl, rep(i, num_real - train_size))
  }
  
  return(list(train = train_ind, 
              test = test_ind, 
              train_lbl = train_lbl, 
              test_lbl = test_lbl)
  )
  
}


train_test_divide_2 <- function(num_classes, num_reals, train_size){
  # "num_classes" - number of different classes (processes) that are considered
  # "num_reals" - number of realisations we consider
  # "percent_train" - train test size in percents 
  
  # Define indexing
  index_all = 1:num_reals
  
  # Define array for storing training and testing indices
  train_ind <- c()
  test_ind <- c()
  
  # Define array for storing training and testing labels
  train_lbl <- c()
  test_lbl <- c()
  
  temp_train <- sample(index_all, size = train_size)
  temp_test <- index_all[!(index_all %in% temp_train)]
  train_ind <- c()
  test_ind <- c()
  train_lbl <- c()
  test_lbl <- c()
  for(i in 1:num_classes){
    train_ind <-c(train_ind, 200*(i-1)+temp_train)
    test_ind <-c(test_ind, 200*(i-1)+temp_test)
    train_lbl <- c(train_lbl, rep(i, train_size))
    test_lbl <- c(test_lbl, rep(i, num_real-train_size))
  }
  
  # temp_train <- sample(index_all, size = train_size)
  # train_ind <- c(temp_train, 200+temp_train, 400+temp_train)
  # temp_test <- index_all[!(index_all %in% temp_train)]
  # test_ind <- c(temp_test, 200+temp_test, 400+temp_test)
  # train_lbl <- c(rep(1, train_size), rep(2, train_size), rep(3, train_size))
  # test_lbl <- c(rep(1, num_real - train_size), rep(2, num_real - train_size), rep(3, num_real - train_size))
  
  return(list(train = train_ind, 
              test = test_ind, 
              train_lbl = train_lbl, 
              test_lbl = test_lbl)
  )
  
}


# ####################################################
# ###################### MAIN ########################
# ####################################################

# Base directory containing the input data
#base_dir <- args[1]
base_dir <- "~/Desktop/Tissues/"

# The radius of the osculating circle
#radius <- args[2]
radius <- 5

# Number of runs
#num_runs <- args[2]
num_runs <- 50

# Number of realisations we consider
#n_reals <- args[4]
n_reals <- 200


# Number of realisations we consider
num_realisations = c(20, 50, 100)

# Considered processes
classes_names <- c("Mamca", "Masto")
classes <- c("MC", "MP")
num_classes <- 2

# Types of characteristics considered
chars <- c("R", "C", "B")#, "B_w", "B_p", "B_c")
#chars <-c("") #, "B_w", "B_p", "B_c")


chars_names <- list(R = "Ratio",
                    C = "Curvature",
                    B = "Both")#,
# B_w = "Both_weighted",
# B_p = "Both_normed",
# B_c = "Both_customized")

# Number of components examined from each realisation
#max_sample = c(10, 20, 1000)
sample_names= c("10", "20", "All")#, "All_perm")

percent_train <- 75

#weights <- readRDS(paste0(base_dir, radius, "_weights/", num_runs, "_r_weights_R_",n_reals, "_real_r_", radius, ".rds"))
#normed <- readRDS(paste0(base_dir, radius, "_weights/", num_runs, "_r_weights_R_",n_reals, "_real_r_", radius, ".rds"))

# Loop through three settings - 20, 50 and 100 realisations
for(num_real in num_realisations){
  #num_real=20
  # Where to store misclaffisication rate
  Misclassification_data <- c()
  
  # Loop through three different arguments: both, curvature and ratio
  for(c in chars){
    
    # Loop through different runs and set the seed for each run to make your partition reproducible
    for(n_run in 1:50){
      set.seed(n_run)
      
      #In each run we plot histogram
      Hist_data_test <- c()
      #  Hist_data_train <- c()
      Hist_test <- c()
      #  Hist_train <- c()
      
      # Randomly choose indices of realisations for training, B, C, R separately -> greater randomness
      train_size <- floor(percent_train * num_real / 100)
      train_test_division <- train_test_divide_2(num_classes, num_real, train_size)
      
      # Loop through different components sample numbers we consider: 10, 20, all (1000)
      for(max_sample_size in sample_names){
        
        val_grp = rep(max_sample_size, times=num_classes^2)
        
        
        #         if(c != "B"){
        # Load semimetrics
        N.dist.matrix <- readRDS(paste0(base_dir, radius, "/", n_run, "_run_Ndist_200_real_r_", radius, "_", c ,"_", max_sample_size, ".rds"))
        # if(c == "B"){
        #   N.dist.matrix <- readRDS(paste0(base_dir, n_run, "_run_Ndist_200_real_r_", radius, "_", max_sample_size, ".rds"))
        #   
        # }
        # else{
        # N.dist.matrix <- readRDS(paste0(base_dir, n_run, "_run_Ndist_", c, "_200_real_r_", radius, "_", max_sample_size, ".rds"))
        # }
        #         }
        #          else{
        #           N.dist.matrix_R <- readRDS(paste0(base_dir, radius, "_updated/", n_run, "_run_Ndist_200_real_r_", radius, "_R_", max_sample_size, ".rds"))
        #          N.dist.matrix_C <- readRDS(paste0(base_dir, radius, "_updated/", n_run, "_run_Ndist_200_real_r_", radius, "_C_", max_sample_size, ".rds"))
        #         w <- weights[n_run, which(sample_names %in% max_sample_size)]
        #        a <- normed[n_run, which(sample_names %in% max_sample_size)]
        #       
        #      N.dist.matrix <- 0.58*N.dist.matrix_R+(1-0.58)*N.dist.matrix_C
        #     
        #  }
        N.dist.matrix_train <- N.dist.matrix[train_test_division$train, train_test_division$train]
        N.dist.matrix_test <- N.dist.matrix[train_test_division$train, train_test_division$test]
        
        ## PREDICTION ##
        res <- funopadi.knn.lcv(train_test_division$train_lbl, N.dist.matrix_train, N.dist.matrix_test,
                                kind.of.kernel = "triangle", twodatasets = 1)
        
        Misclassif <- sum(res$Predicted.classnumber != train_test_division$test_lbl)/(num_classes*train_size)
        Misclassif_df <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif)
        Misclassification_data <- rbind(Misclassification_data, Misclassif_df)
        
        true_class_names <- c()
        pred_class_names <- c()
        for(cl in 1:num_classes){
          true_class_names <- c(true_class_names, rep(classes[cl], num_classes ))
          pred_class_names <- c(pred_class_names, classes)
        }
        
        Hist_test_data <- data.frame("True class" = true_class_names,
                                     "Classified as" = pred_class_names,"Number of components" = val_grp ,"Count" = unlist(misclassification_stats(train_test_division$test_lbl, res$Predicted.classnumber)))
        Hist_test <-rbind(Hist_test, Hist_test_data)
      } # DONE - Loop through different components sample numbers we consider: 10, 20, all (1000)
      Hist_test %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
        ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      ggsave(paste(base_dir, "Results_knn/", radius, "/", n_run,"_part_Hist_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
    } # DONE - Loop through different runs
  } # DONE - Loop through three different arguments - both, curvature, ratio
  
  Misclassification_data %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
    ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
  
  ggsave(paste(base_dir, "Results_knn/", radius, "/", n_run, "_part_Box_", num_real, "_", num_runs, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
  write.csv(Misclassification_data, file=paste0(base_dir, "Results_knn/", radius, "/", n_run,"_part_Misclassification_data_", num_real, ".csv"))
  
} # DONE - Loop through three settings - 20, 50 and 100 realisations

