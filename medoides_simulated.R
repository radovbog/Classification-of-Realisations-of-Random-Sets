library(ggplot2)
library(readr)
library(tidyverse)
library(mclust)  # NMI, V-measure
library(clevr)    
library(cluster)
library(combinat)
source("inserting.R")

# Base directory containing the input data
#base_dir <- args[1]
base_dir <- "~/Desktop/"

# The radius of the osculating circle
#radius <- args[2]
radius <- 5

# Number of runs
#num_runs <- args[2]
num_runs <- 50

# Train test size in percents
percent_train = 100

##### NOTE : WHEN LOOKING AT THE RESULTS CONSIDER ONLY ONES MARKED WITH THE NUMBER OF COMPONENTS YOU CHOSE TO CONSIDER!

for(num_real in num_realisations){
  
  # Where to store misclaffisication rate
  # Misclassification_data <- c()
  # Misclassification_data_corr <- c()
  Misclassification_train_data <- c()
  Misclassification_train_data_corr <- c()
  
  for(c in chars){
    
    # Loop through different runs and set the seed for each run to make your partition reproducible
    for(n_run in 1:num_runs){
      
      set.seed(n_run)
      #In each run we plot histogram
      Hist_data_train <- c()
      Hist_data_train_corr <- c()
      # Hist_data_test <- c()
      # Hist_data_test_corr <- c()
      Hist_train <- c()
      Hist_train_corr <- c()
      # Hist_test <- c()
      # Hist_test_corr <- c()
      
      # Randomly choose indices of realisations for training, B, C, R separately -> greater randomness
      train_size <- floor(percent_train * num_real / 100)
      test_size <- num_real-train_size
      train_test_division <- train_test_divide_2(num_classes, num_real, train_size)
      labels_train <- rep(0, 3*train_size)
      labels_test <- rep(0, 3*test_size)
      classes_train <- train_test_division$train_lbl
      classes_test <- train_test_division$test_lbl
      
      # Loop through different components sample numbers we consider: 10, 20, All 
      for(max_sample_size in sample_names){
        val_grp = rep(max_sample_size, times=9)
        
        # Load distance matrix
        N.dist.matrix <- readRDS(paste0(base_dir, radius, "/", n_run, "_run_Ndist_200_real_r_", radius, "_", c ,"_", max_sample_size, ".rds"))
        
        # Separate train set
        N.dist.matrix_train <- N.dist.matrix[train_test_division$train, train_test_division$train]
        
        # Initialise medoid indices
        med_train <- c(sample(train_test_division$train[1:train_size], 1), sample(train_test_division$train[(train_size+1):(train_size+train_size)], 1), sample(train_test_division$train[(train_size+train_size+1):(train_size+train_size+train_size)], 1))
        med_train_previous <- med_train
        med_train_previous2 <- med_train
        
        
        # Initialization when there are randomly-sized clusters
        # Assign class based on initialisation
        x_train <- vector("list", length = nrow(N.dist.matrix_train))
        
        # Put them all in class 1 and set min_dist_train to be distance from it to med_train[1]
        for (i in 1:nrow(N.dist.matrix_train)){
          x_train[[i]] <- REALISATION
          x_train[[i]]$cls <- 1
          min_dist_train <- N.dist.matrix_train[i, match(med_train[1], train_test_division$train)]
          for (cl in 2:n_classes) {
            
            # If distance from it to med_train[cl] is less than min_dist_train, cls: 1->cl
            if (N.dist.matrix_train[i, match(med_train[cl], train_test_division$train)] < min_dist_train) {
              x_train[[i]]$cls <- cl
              min_dist_train <- N.dist.matrix_train[i, match(med_train[cl], train_test_division$train)]
            }
          }
        }
        
        #   ** FROM HERE
        k <- 1
        while (k > 0) {
          # For each realisation x[i] calculate sum of distances to all others x[j] with the same class
          k <- n_classes
          for (i in 1:nrow(N.dist.matrix_train)) {
            x_train[[i]]$sum_dist <- sum(N.dist.matrix_train[i, x_train[[i]]$cls == x_train[[1]]$cls])
          }
          
          # Recalculate medoides for each class
          for (cls_1 in 1:n_classes) {
            min_sum_dist <- Inf
            for (i in 1:nrow(N.dist.matrix_train)) {
              if (x_train[[i]]$cls == cls_1  && x_train[[i]]$sum_dist < min_sum_dist) {
                med_train[cls_1] <- i
                min_sum_dist <- x_train[[i]]$sum_dist
              }
            }
            #   print(k)
            if (med_train[cls_1] == med_train_previous[cls_1]) k <- k - 1
            med_train_previous[cls_1] <- med_train[cls_1]
          }
        }
        
        # Collect trained labels
        for(d in 1:length(train_test_division$train)){
          labels_train[d]<-x_train[[d]]$cls
        }
        
        # ** TO HERE
        
        # Find permutation of labels with smallest mistake
        perms <- array(permn(3))
        misclassif_train <-rep(Inf, 6)
        for(perr in 1:6){
          temp_train <- perms[[perr]]
          misclassif_train[perr] <- sum(rotate_labels(labels_train, temp_train[1], temp_train[2], temp_train[3]) != classes_train)/(n_classes*train_size)
        }
        perm_train_id <- which.min(misclassif_train)
        perm_train_id <- perm_train_id[1]
        temp_train <- perms[[perm_train_id]]
        med_train_corr <- c(med_train[temp_train[1]], med_train[temp_train[2]], med_train[temp_train[3]])
        
        # Store misclassification
        Misclassif_train <- sum(labels_train != classes_train)/(n_classes*train_size)
        Misclassif_train_df <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif_train)
        Misclassification_train_data <- rbind(Misclassification_train_data, Misclassif_train_df)
        
        # Store misclassification with label correction
        Misclassif_train_corr <- sum(rotate_labels(labels_train, temp_train[1], temp_train[2], temp_train[3]) != classes_train)/(n_classes*train_size)
        Misclassif_train_df_corr <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif_train_corr)
        Misclassification_train_data_corr <- rbind(Misclassification_train_data_corr, Misclassif_train_df_corr)
        
        # Plot histograms
        Hist_train_data <- data.frame("True class" = true_class_names,
                                      "Classified as" = pred_class_names,"Number of components" = val_grp ,"Count" = unlist(misclassification_stats(classes_train, labels_train)))
        Hist_train <-rbind(Hist_train, Hist_train_data)
        
        # Plot histograms with label correction
        Hist_train_data_corr <- data.frame("True class" = true_class_names,
                                           "Classified as" = pred_class_names,"Number of components" = val_grp ,"Count" = unlist(misclassification_stats(classes_train, rotate_labels(labels_train, temp_train[1], temp_train[2], temp_train[3]))))
        Hist_train_corr <-rbind(Hist_train_corr, Hist_train_data_corr)
        
        # # *** FROM HERE
        # # Assign class based on trained medoids
        # coupled = c(train_test_division$test, med_train)
        # coupled = sort(coupled)
        # N.dist.matrix_test <- N.dist.matrix[coupled, coupled]
        # x_test <- vector("list", length = nrow(N.dist.matrix_test))
        # 
        # for (p in 1:nrow(N.dist.matrix_test)) {
        #   x_test[[p]] <- REALISATION
        #   x_test[[p]]$cls <- 1
        #   iddx <- match(med_train[1], coupled)
        #   min_dist_test <- N.dist.matrix_test[p, iddx]
        #   for (cls_2 in 2:n_classes) {
        #     if (N.dist.matrix_test[p, match(med_train[cls_2], coupled)] < min_dist_test) {
        #       x_test[[p]]$cls <- cls_2 
        #       min_dist_test <- N.dist.matrix_test[p, match(med_train[cls_2], coupled)]
        #     }
        #   }
        # }
        # # *** TO HERE
        
        
        
        # for (p in 1:nrow(N.dist.matrix_test)) {
        #   x_test[[p]] <- REALISATION
        #   
        #   # Distances to trained medoids
        #   dists <- sapply(1:n_classes, function(cls) {
        #     N.dist.matrix_test[p, match(med_train[cls], coupled)]
        #   })
        #   
        #   # Prefer closest medoids with available space
        #   preferred_order <- order(dists)
        #   assigned <- FALSE
        #   for (cl in preferred_order) {
        #     if (cluster_sizes_test[cl] < upper_bound_test) {
        #       x_test[[p]]$cls <- cl
        #       cluster_sizes_test[cl] <- cluster_sizes_test[cl] + 1
        #       assigned <- TRUE
        #       break
        #     }
        #   }
        #   
        #   # Fallback: assign to smallest cluster
        #   if (!assigned) {
        #     cl <- which.min(cluster_sizes_test)
        #     x_test[[p]]$cls <- cl
        #     cluster_sizes_test[cl] <- cluster_sizes_test[cl] + 1
        #   }
        # }
        
        
        # for(d in 1:length(train_test_division$test)){
        #   labels_test[d]<-x_test[[d]]$cls
        # }
        # 
        # 
        # accu <- misclassification_stats(classes_train, labels_train)
        # 
        # # Adjusted Rand Index
        # # ari <- adjustedRandIndex(labels_test, classes_test)
        # # ari <- adjustedRandIndex(labels_train, classes_train)
        # 
        # perms <- array(permn(3))
        # misclassif <-rep(Inf, 6)
        # for(per in 1:6){
        #   temp <- perms[[per]]
        #   misclassif[per] <- sum(rotate_labels(labels_test, temp[1], temp[2], temp[3]) != classes_test)/(n_classes*test_size)
        # }
        # perm_id <- which.min(misclassif)
        # perm_id <- perm_id[1]
        # temp <- perms[[perm_id]]
        # 
        # clevr::v_measure(labels_test, classes_test)
        # clevr::v_measure(labels_train, classes_train)
        # 
        # dmat <- as.dist(N.dist.matrix_train)  # silhouette expects 'dist' class
        # 
        # sil <- silhouette(labels_train, dmat)
        # avg_sil <- mean(sil[, 3])
        
        
        
        # #    print("ok4")
        # 
        # # No label correction
        # Misclassif <- sum(labels_test != classes_test)/(n_classes*test_size)
        # Misclassif_df <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif)
        # Misclassification_data <- rbind(Misclassification_data, Misclassif_df)
        # 
        # # Label correction
        # Misclassif_corr <- sum(rotate_labels(labels_test, temp[1], temp[2], temp[3]) != classes_test)/(n_classes*test_size)
        # Misclassif_df_corr <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif_corr)
        # Misclassification_data_corr <- rbind(Misclassification_data_corr, Misclassif_df_corr)
        # 
        # 
        # # No label correction
        # Hist_test_data <- data.frame("True class" = true_class_names,
        #                              "Classified as" = pred_class_names,"Number of components" = val_grp ,"Count" = unlist(misclassification_stats(classes_test, labels_test)))
        # Hist_test <-rbind(Hist_test, Hist_test_data)
        # 
        # # Label correction
        # Hist_test_data_corr <- data.frame("True class" = true_class_names,
        #                                   "Classified as" = pred_class_names,"Number of components" = val_grp ,"Count" = unlist(misclassification_stats(classes_test, rotate_labels(labels_test, temp[1], temp[2], temp[3]))))
        # Hist_test_corr <-rbind(Hist_test_corr, Hist_test_data_corr)
        # 
        
      } # DONE - Loop through different components sample numbers we consider: 10, 20, all (1000)
      
      # No label correction
      Hist_train %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
        ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      ggsave(paste(base_dir, "Results_kmed_full_", radius, "/", n_run,"_part_Hist_train_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
      
      # Label correction
      Hist_train_corr %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
        ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      ggsave(paste(base_dir, "Results_kmed_full_", radius, "/", n_run,"_part_Hist_train_corr_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
      
      # # No label correction
      # Hist_test %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
      #   ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      # ggsave(paste(base_dir, "Results_kmed_", radius, "/", n_run,"_part_Hist_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
      # 
      # # Label correction
      # Hist_test_corr %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
      #   ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      # ggsave(paste(base_dir, "Results_kmed_", radius, "/", n_run,"_part_Hist_corr_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
      # 
    } # DONE - Loop through different runs
  } # DONE - Loop through three different arguments - both, curvature, ratio
  
  # write.csv(Misclassification_data, file=paste0(base_dir, "Results_kmed_full_", radius, "/", n_run,"_part_Misclassification_data_", num_real, ".csv"))
  # write.csv(Misclassification_data_corr, file=paste0(base_dir, "Results_kmed_full_", radius, "/", n_run,"_part_Misclassification_data_corr_", num_real, ".csv"))
  write.csv(Misclassification_train_data, file=paste0(base_dir, "Results_kmed_full_", radius, "/", n_run,"_part_Misclassification_data_train_", num_real, ".csv"))
  write.csv(Misclassification_train_data_corr, file=paste0(base_dir, "Results_kmed_full_", radius, "/", n_run,"_part_Misclassification_data_train_corr_", num_real, ".csv"))
  
  Misclassification_train_data %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
    ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
  ggsave(paste(base_dir, "Results_kmed_full_", radius, "/", n_run, "_runs_Box_", num_real, "_real_", radius, "_radius_", "train_", ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")  
  
  Misclassification_train_data_corr %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
    ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
  ggsave(paste(base_dir, "Results_kmed_full_", radius, "/", n_run, "_runs_Box_", num_real, "_real_", radius, "_radius_", "train_", "corr_", ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
  
  
} # DONE - Loop through three settings - 20, 50 and 100 realisations