################################################
#
################################################

  simulated_cohort_directory <- "~/shexport/accelerometer/data/"

cohort_formatted <- list.files(simulated_cohort_directory)[8]
cohort_data <- read.table(file = paste0(simulated_cohort_directory, cohort_formatted), 
                          sep = ",", header = TRUE, stringsAsFactors = FALSE)

complete_cohort <- cohort_data

id_var <- "fID"
dim_vars <- c("season", "hour", "weekday", "Date")
dim_vars <- c("weekday", "hour")
sample_vars <- c("Axis1", "Axis2", "Axis3")

################################################
# This function takes a data frame and creates
# and index variable based on user-specification

# dim_vars is an vector of characters indicating
#   teh variables which define the sample units
#   ex. dim_vars <- c("weekday", "hour") will
#   sample hours within weekdays
#   order matters; vars should be nested in 
#   the variable to their left

# sample_vars is a character vector specifying
#   which variables to sample

# n_samples is an integer specifying how many
#   subjects to sample from complete cohort
################################################

simulateAccelerate <- function(complete_cohort, dim_vars, sample_vars, n_samples) {
  library(plyr)
  library(dplyr)
  #create row ID variable
  complete_cohort$row_id <- 1:nrow(complete_cohort)
  
  sampled_cohort <- ddply(cohort_data, dim_vars, function(.cohort_subset, .n_samples, .sample_vars) {
    
    
    .sampled_rows <- sample(x = 1:nrow(.cohort_subset), size = .n_samples, replace = TRUE)
    
    .sampled_cohort <- .cohort_subset[ .sampled_rows, .sample_vars ]
    .sampled_cohort$id_var <- 1:.n_samples
    
    .sampled_cohort
  }, n_samples, sample_vars)
  
  sampled_cohort <- arrange_( sampled_cohort, c("id_var", dim_vars) ) %>%
    select_( .dots =  c("id_var", dim_vars, sample_vars) )
            
  return(sampled_cohort)
}
  
  
  
  
  
}
                           