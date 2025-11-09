rotate_labels <-function(labels, b, c, r) {
  mapping <- c('1' = b, '2' = c, '3' = r)
  new_labels<- mapping[as.character(labels)]
  
  return(as.numeric(new_labels))
}


misclassification_stats <- function(y_true, y_pred) {
  # Calculate and print misclassifications on the test set
  classified_B_as_B <- sum((y_true == 1) & (y_pred == 1))
  misclassified_B_as_C <- sum((y_true == 1) & (y_pred == 2))
  misclassified_B_as_R <- sum((y_true == 1) & (y_pred == 3))
  misclassified_C_as_B <- sum((y_true == 2) & (y_pred == 1))
  classified_C_as_C <- sum((y_true == 2) & (y_pred == 2))
  misclassified_C_as_R <- sum((y_true == 2) & (y_pred == 3))
  misclassified_R_as_B <- sum((y_true == 3) & (y_pred == 1))
  misclassified_R_as_C <- sum((y_true == 3) & (y_pred == 2))
  classified_R_as_R <- sum((y_true == 3) & (y_pred == 3))
  accuracy <- (classified_B_as_B+classified_C_as_C+classified_R_as_R)/(length(y_true))
  
  # Return the calculated misclassifications ..... UNCOMMENT IF YOU WANT DETAILED RATES
  return(list(
    "B_as_B" = classified_B_as_B,
     "B_as_C" = misclassified_B_as_C,
     "B_as_R" = misclassified_B_as_R,
     "C_as_B" = misclassified_C_as_B,
     "C_as_C" = classified_C_as_C,
     "C_as_R" = misclassified_C_as_R,
     "R_as_B" = misclassified_R_as_B,
     "R_as_C" = misclassified_R_as_C,
     "R_as_R" = classified_R_as_R
   # "accuracy" = accuracy
  ))
}


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
  train_ind <- c(temp_train, 200+temp_train, 400+temp_train)
  temp_test <- index_all[!(index_all %in% temp_train)]
  test_ind <- c(temp_test, 200+temp_test, 400+temp_test)
  train_lbl <- c(rep(1, train_size), rep(2, train_size), rep(3, train_size))
  test_lbl <- c(rep(1, num_real - train_size), rep(2, num_real - train_size), rep(3, num_real - train_size))
  
  # return(list(train = train_ind, 
  #             test = test_ind, 
  #             train_lbl = train_lbl, 
  #             test_lbl = test_lbl)
  # )
  
  return(list(train = sort(train_ind), 
              test = sort(test_ind), 
              train_lbl = sort(train_lbl), 
              test_lbl = sort(test_lbl))
  )
  
}

true_class_names = c("B", "B", "B", "C", "C", "C", "R", "R", "R")
pred_class_names = c("B", "C", "R", "B", "C", "R", "B", "C", "R")

REALISATION <- list(cls = integer(), sum_dist = numeric())


# Storing accuaracy
accu <-replicate(9, numeric(18))

# Number of components that were examined from each realisation
sample_names= c("10", "20", "All")

# Types of characteristics considered
chars <- c("B", "C", "R")
chars_names <- list(R = "Ratio",
                    C = "Curvature",
                    B = "Both")

# Considered processes
classes_names <- c("Boolean", "Cluster", "Repulsive")
classes <- c("B", "C", "R")
n_classes <- 3

# Number of realisations we consider
n_reals <- 200

# Number of realisations we consider
num_realisations = c(20, 50, 100)