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

indexAvailableData <- function(complete_cohort, id_var, dim_vars) {
  
  # .(fID, season, hour, weekday, Date),
  complete_index <- ddply(complete_cohort, c(id_var, dim_vars), function(.data_block) {
    cat("Indexing wear hours for", .subject_hour$fID[1], "\n")
    
    if(  ( max(.subject_hour$nonwear) == 0 ) & ( nrow(.subject_hour) == 60 ) ){ 
      .tor <- data.frame(day_hour = paste0( .subject_hour$weekday[1], "_", 
                                            str_pad(.subject_hour$hour[1], width = 2, side = "left", pad = "0"), "_",
                                            str_pad(.subject_hour$doy[1], width = 3, side = "left", pad = "0") 
      ),
      fID_day_hour = paste0(.subject_hour$fID[1], "_", .subject_hour$weekday[1], "_", 
                            str_pad(.subject_hour$hour[1], width = 2, side = "left", pad = "0"), "_",
                            str_pad(.subject_hour$doy[1], width = 3, side = "left", pad = "0") 
      ),
      stringsAsFactors = FALSE)
      
      return(.tor) 
    }
  })
  
  
  
  
}
generateCohort <- function(complete_cohort, scale, size, block, n_blocks) {
  
  
  
  
  
  
  
  
  
}
                           