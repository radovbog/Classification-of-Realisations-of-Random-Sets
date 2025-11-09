library(ggplot2)
library(readr)
library(tidyverse)
source("inserting.R")

# Base directory containing the input data
#base_dir <- args[1]
base_dir <- "~/Desktop/"

# The radius of the osculating circle
#radius <- args[2]
radius <- 3

# Number of runs
#num_runs <- args[2]
n_run <- 50

p <- "Results_kmed_"
pp <- "_part_Misclassification_data_"


f <- "full_"
r <- "restriction_"
d1 <- paste0(radius)
d2 <- paste0(r, radius)
d3 <- paste0(f, radius)
d4 <- paste0(f, r, radius)
d<- c(d1, d2, d3, d4)

tr <- "train_"
c <- "corr_"

t1 <- ""
t2 <- paste0(c)
t3 <- paste0(tr)
t4 <- paste0(tr, c)
t <- c(t1, t2, t3, t4)


for(num_real in num_realisations){
  
  ppp <- paste0(num_real, ".csv")

  for (i in 1:4){
    for(j in 1:4){
      
      if(i<3){
        f <- paste0(base_dir, p, d[[i]], "/", n_run, pp, t[[j]], ppp)
        Misclassification_data <- read_csv(f, show_col_types = FALSE)
        
        Misclassification_data %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
          ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
        
        ggsave(paste(base_dir, "Box_Results/", n_run, "_runs_Box_", num_real, "_real_", d[[i]], "_radius_", t[[j]], ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
      }
      else{
        if(j>3){
          f <- paste0(base_dir, p, d[[i]], "/", n_run, pp, t[[j]], ppp)
          Misclassification_data <- read_csv(f, show_col_types = FALSE)
          
          Misclassification_data %>% mutate(Number.of.components = factor(Number.of.components)) %>% 
            ggplot(aes(x=Number.of.components,y=Value, fill=Number.of.components)) + geom_boxplot()+ theme(legend.position="top",panel.background = element_rect(fill = 'white', colour = 'black')) + scale_fill_brewer(palette="Set1") + facet_grid(.~Characteristic.considered)
          
          ggsave(paste(base_dir, "Box_Results/", n_run, "_runs_Box_", num_real, "_real_", d[[i]], "_radius_", t[[j]], ".png", sep = ""),plot = last_plot(),width = 18,height = 7,units = "cm")
        }
      }
     }
    

    
  }
  
}