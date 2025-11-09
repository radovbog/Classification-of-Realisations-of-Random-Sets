library(ggplot2)
library(readr)
library(tidyverse)
library(mclust)  # NMI, V-measure
library(clevr)    
library(cluster)
library(combinat)
source("inserting_tissues.R")

library(aricode)
library(flexclust)
library(fpc)



# ####################################################
# ###################### MAIN ########################
# ####################################################

# Base directory containing the input data
#base_dir <- args[1]
base_dir <- "~/Desktop/Tissues/"

# The radius of the osculating circle
#radius <- args[2]
radius <- 3

# Number of runs
#num_runs <- args[2]
num_runs <- 50

# Train test size in percents
percent_train = 100


# Loop through three settings - 20, 50 and 100 realisations
for(num_real in num_realisations){
  #num_real=20
  # Where to store misclaffisication rate
  Misclassification_train_data <- c()
  Misclassification_train_data_corr <- c()
  
  # Loop through three different arguments: both, curvature and ratio
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
      train_test_division <- train_test_divide_2(n_classes, num_real, train_size)
      labels_train <- rep(0, n_classes*train_size)
      labels_test <- rep(0, n_classes*test_size)
      classes_train <- train_test_division$train_lbl
      classes_test <- train_test_division$test_lbl
      
      # Loop through different components sample numbers we consider: 10, 20, all (1000)
      for(max_sample_size in sample_names){
        
        val_grp = rep(max_sample_size, times=4)
        
        
        #         if(c != "B"){
        # Load semimetrics
        # Load distance matrix
        N.dist.matrix <- readRDS(paste0(base_dir, radius, "/", n_run, "_run_Ndist_200_real_r_", radius, "_", c ,"_", max_sample_size, ".rds"))
        
        # Separate train set
        N.dist.matrix_train <- N.dist.matrix[train_test_division$train, train_test_division$train]
        
        #  N.distance <- as.dist(N.dist.matrix)
        N.distance.train <- as.dist(N.dist.matrix_train)
        
        # Prediction
        # hc<-hclust(N.distance, method = "ward.D2")
        hc_train<-hclust(N.distance.train, method = "ward.D2")
        #    hc_test<-hclust(N.distance.test, method = "ward.D2")
        
        #clusters <- cutree(hc, k = n_classes)
        clusters_train <- cutree(hc_train, k = n_classes)
        #   clusters_test <- cutree(hc_test, k = num_classes)
        
        #   accu[n_run, 1]<-as.matrix(misclassification_stats(true_labels, clusters))
        #   accu_train[n_run, 1]<-as.matrix(misclassification_stats(true_labels, clusters))
        # #  accu_test[n_run, 1]<-as.matrix(misclassification_stats(true_labels, clusters))
        #   
        # ###### MODEL EVALUATIONS
        # 
        # # Cophenetic correlation (Evaluation of the model without using true labels)
        # coph_dist <- cophenetic(hc)
        # cor_cophenetic <- cor(N.distance, coph_dist)
        # print(paste("Cophenetic Correlation:", round(cor_cophenetic, 3)))
        # 
        # # Silhouete score (Evaluation of the model without using true labels)
        # sil <- silhouette(clusters, N.distance)
        # mean_sil <- mean(sil[, 3])
        # print(paste("Average Silhouette Width:", round(mean_sil, 3)))
        # 
        # # Adjusted Rand Index (ARI) (Evaluation using true labels)
        # ari <- adjustedRandIndex(true_labels, clusters)
        # print(paste("Adjusted Rand Index:", round(ari, 3)))
        # 
        # # Normalized Mutual Information (NMI) (Evaluation using true labels)
        # nmi <- NMI(true_labels, clusters)
        # print(paste("Normalized Mutual Information:", round(nmi, 3)))
        # 
        # plot(hc, labels = FALSE, main = "Dendrogram")
        # rect.hclust(hc, k = num_classes+3, border = 2:4)  # draw rectangles for clusters
        # leaf_order <- hc$order
        # cluster_labels <- clusters[leaf_order]
        # # Add text below each leaf
        # midpoints <- seq_along(cluster_labels)
        # text(x = midpoints, y = -2, labels = cluster_labels, col = cluster_labels, cex = 0.7)
        # 
        # 
        # accu[idd,1]<-as.matrix(misclassification_stats(c20, memb20))
        # miss <-1-accu
        # 
        ## PREDICTION ##
        
        
        # Collect trained labels
        for(d in 1:length(train_test_division$train)){
          labels_train[d]<-clusters_train[d]
        }
        
        # ** TO HERE
        
        # Find permutation of labels with smallest mistake
        perms <- array(permn(3))
        misclassif_train <-rep(Inf, 6)
        for(perr in 1:6){
          temp_train <- perms[[perr]]
          misclassif_train[perr] <- sum(rotate_labels(labels_train, temp_train[1], temp_train[2]) != classes_train)/(n_classes*train_size)
        }
        perm_train_id <- which.min(misclassif_train)
        perm_train_id <- perm_train_id[1]
        temp_train <- perms[[perm_train_id]]
        # med_train_corr <- c(med_train[temp_train[1]], med_train[temp_train[2]], med_train[temp_train[3]])
        
        # Store misclassification
        Misclassif_train <- sum(labels_train != classes_train)/(n_classes*train_size)
        Misclassif_train_df <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif_train)
        Misclassification_train_data <- rbind(Misclassification_train_data, Misclassif_train_df)
        
        # Store misclassification with label correction
        Misclassif_train_corr <- sum(rotate_labels(labels_train, temp_train[1], temp_train[2]) != classes_train)/(n_classes*train_size)
        Misclassif_train_df_corr <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif_train_corr)
        Misclassification_train_data_corr <- rbind(Misclassification_train_data_corr, Misclassif_train_df_corr)
        
        # Plot histograms
        Hist_train_data <- data.frame("True class" = true_class_names,
                                      "Classified as" = pred_class_names,"Number of components" = val_grp ,"Count" = unlist(misclassification_stats(classes_train, labels_train)))
        Hist_train <-rbind(Hist_train, Hist_train_data)
        
        # Plot histograms with label correction
        Hist_train_data_corr <- data.frame("True class" = true_class_names,
                                           "Classified as" = pred_class_names,"Number of components" = val_grp ,"Count" = unlist(misclassification_stats(classes_train, rotate_labels(labels_train, temp_train[1], temp_train[2]))))
        Hist_train_corr <-rbind(Hist_train_corr, Hist_train_data_corr)
        
        
        
        # Misclassif <- sum(res$Predicted.classnumber != train_test_division$test_lbl)/(num_classes*train_size)
        # Misclassif_df <-data.frame("Characteristic considered" = chars_names[[c]], "Number of components" = max_sample_size, "Value" = Misclassif)
        # Misclassification_data <- rbind(Misclassification_data, Misclassif_df)
        # 
        # true_class_names <- c()
        # pred_class_names <- c()
        # for(cl in 1:num_classes){
        #   true_class_names <- c(true_class_names, rep(classes[cl], num_classes ))
        #   pred_class_names <- c(pred_class_names, classes)
        # }
        
      } # DONE - Loop through different components sample numbers we consider: 10, 20, all (1000)
      
      Hist_train %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
        ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      ggsave(paste(base_dir, "Results_hc_full_", radius, "/", n_run,"_part_Hist_train_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
      
      # Label correction
      Hist_train_corr %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
        ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      ggsave(paste(base_dir, "Results_hc_full_", radius, "/", n_run,"_part_Hist_train_corr_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
      
      
      #  Hist_test %>% mutate(Classified.as = factor(Classified.as, levels=classes)) %>% 
      #   ggplot() + geom_bar(aes(x=True.class, y=Count, fill = Classified.as), stat = "identity", position = "dodge") + facet_grid(.~Number.of.components)
      # ggsave(paste(base_dir, "Results/", n_run,"_part_Hist_", num_real, "_", chars_names[[c]], "_", n_run, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
    } # DONE - Loop through different runs
  } # DONE - Loop through three different arguments - both, curvature, ratio
  
  # Misclassification_data %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
  #   ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
  # 
  # ggsave(paste(base_dir, "Results/", n_run, "_part_Box_", num_real, "_", num_runs, ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
  # write.csv(Misclassification_data, file=paste0(base_dir, "Results/", n_run,"_part_Misclassification_data_", num_real, ".csv"))
  # 
  
  
  
  write.csv(Misclassification_train_data, file=paste0(base_dir, "Results_hc_full_", radius, "/", n_run,"_part_Misclassification_data_train_", num_real, ".csv"))
  write.csv(Misclassification_train_data_corr, file=paste0(base_dir, "Results_hc_full_", radius, "/", n_run,"_part_Misclassification_data_train_corr_", num_real, ".csv"))
  
  Misclassification_train_data %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
    ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
  ggsave(paste(base_dir, "Results_hc_full_", radius, "/", n_run, "_runs_Box_", num_real, "_real_", radius, "_radius_", "train_", ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")  
  
  Misclassification_train_data_corr %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
    ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
  ggsave(paste(base_dir, "Results_hc_full_", radius, "/", n_run, "_runs_Box_", num_real, "_real_", radius, "_radius_", "train_", "corr_", ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
  
  
  
} # DONE - Loop through three settings - 20, 50 and 100 realisations

# sum(clusters[1:200] == 1) #96 (80)
# sum(clusters[1:200] == 2) #103 2=B (103) THIS 2=B
# sum(clusters[1:200] == 3) # 1 (16) NOT. R
# sum(clusters[1:200] == 4) # (1) NOT. C
# sum(clusters[1:200] == 5) # (1) NOT. C
# sum(clusters[1:200] == 6) # (1) NOT. C
# #(31, 103, 16, 49, 1, 0) THIS 2 acc 0.5
# 
# sum(clusters[201:400] == 1) #113 (88) 
# sum(clusters[201:400] == 2) #4 (4)  NOT. B
# sum(clusters[201:400] == 3) #83 3=C (25) NOT. R
# sum(clusters[201:400] == 4) #(83)  4=C
# sum(clusters[201:400] == 5) #(83)  4=C
# sum(clusters[201:400] == 6) #(83)  4=C
# #(11, 4, 25, 77, 63, 20) THIS 5+6+4 acc 0.8
# 
# sum(clusters[401:600] == 1) #159 1=R (56)
# sum(clusters[401:600] == 2) #1 (1) NOT. B
# sum(clusters[401:600] == 3) #40 (103) THIS 3=R
# sum(clusters[401:600] == 4) # (40) NOT.C
# sum(clusters[401:600] == 5) # (40) NOT.C
# sum(clusters[401:600] == 6) # (40) NOT.C
# #(32, 1, 103, 24, 39, 1) THIS 3+1 acc 0.67
# 
