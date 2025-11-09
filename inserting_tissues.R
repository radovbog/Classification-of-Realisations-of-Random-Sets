rotate_labels <-function(labels, MC, MP) {
  mapping <- c('1' = MC, '2' = MP)
  new_labels<- mapping[as.character(labels)]
  
  return(as.numeric(new_labels))
}


misclassification_stats <- function(y_true, y_pred) {
  # Calculate and print misclassifications on the test set
  classified_MC_as_MC <- sum((y_true == 1) & (y_pred == 1))
  misclassified_MC_as_MP <- sum((y_true == 1) & (y_pred == 2))
  misclassified_MP_as_MC <- sum((y_true == 2) & (y_pred == 1))
  classified_MP_as_MP <- sum((y_true == 2) & (y_pred == 2))
  accuracy <- (classified_MC_as_MC+classified_MP_as_MP)/(length(y_true))
  
  # Return the calculated misclassifications ..... UNCOMMENT IF YOU WANT DETAILED RATES
  return(list(
    "MC_as_MC" = classified_MC_as_MC,
    "MC_as_MP" = misclassified_MC_as_MP,
    "MP_as_MC" = misclassified_MP_as_MC,
    "MP_as_MP" = classified_MP_as_MP
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
  train_ind <- c(temp_train, 200+temp_train)
  temp_test <- index_all[!(index_all %in% temp_train)]
  test_ind <- c(temp_test, 200+temp_test)
  train_lbl <- c(rep(1, train_size), rep(2, train_size))
  test_lbl <- c(rep(1, num_real - train_size), rep(2, num_real - train_size))
  
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

true_class_names = c("MC", "MC", "MP", "MP")
pred_class_names = c("MC", "MP", "MC", "MP")

REALISATION <- list(cls = integer(), sum_dist = numeric())


# Storing accuaracy
accu <-replicate(4, numeric(4))

# Number of components that were examined from each realisation
sample_names= c("10", "20", "All")

# Types of characteristics considered
chars <- c("B", "C", "R")
chars_names <- list(R = "Ratio",
                    C = "Curvature",
                    B = "Both")

# Considered processes
classes_names <- c("Mamca", "Masto")
classes <- c("MC", "MP")
n_classes <- 2

# Number of realisations we consider
n_reals <- 200

# Number of realisations we consider
num_realisations = c(20, 50, 100)