if(FALSE) {
  rm(list = ls())
  gc()
  source("~/.Rprofile")
}
system(paste0("scp /Volumes/QSU/Datasets/SPRC/Robinson/imputationProject/src/r/simulateCohortSherlock.R ",
              "kikapp@sherlock:/share/PI/manishad/Youth_Group/GOALS/src/r/") )

system(paste0("scp /Volumes/QSU/Datasets/SPRC/Robinson/imputationProject/src/sbatch/* ",
              "kikapp@sherlock:/share/PI/manishad/Youth_Group/GOALS/src/sbatch/") )

system(paste0("scp kikapp@sherlock:", output_directory, "sim_goals_0001.csv ",
              "~/shexport/accelerometer/data/") )
openFilesInDirectory <- 
  function(directory, match_string, merge = FALSE, delim_str =",", na.strings = ".", header = T, fill=T, skip = 0) {
  
  file_array <-  paste0(directory, "/", list.files(directory)[grep(pattern=match_string, list.files(directory))])
  
  data_list <- llply(file_array, function(file_path, delim_str) {
    cat(file_path, "\n")
    to_return <- read.table(file = file_path, header = header, sep = delim_str, stringsAsFactors = FALSE, fill=fill, quote="\"", na.strings = na.strings, skip = skip )
    to_return["loaded_file_name"] <- tail(strsplit(file_path, "/")[[1]],1)
    return(to_return)
  }, delim_str)
  
  if(merge |  length(file_array) == 1) {
    data_list <- ldply(data_list, identity)
  }
  
  return(data_list)
}


number_cores <- 16

require(doSNOW)
require(plyr)

cl<-makeCluster(number_cores)
registerDoSNOW(cl)

number_cores
getDoParWorkers()

clusterApply(cl, seq(along=cl), function(.id) {
  
  set.seed(100*(2^.id))
  
  #load required packages
  
  library(lubridate)
  library(plyr)
  # library(dplyr) not compatible with sherlock's R version
  library(scales)
  library(stringr)
  library(PhysicalActivity)
  source("/share/PI/manishad/Youth_Group/GOALS/src/r/choiFixed.R")
  
} )

########################################
# this script loads 1 minute choi-classified 
# data and models nonwear data patterns as 
# a function of season, time of day
########################################

# setwd("/scratch/PI/manishad/Youth_Group/GOALS/imputationProject/input/oneMinChoi")

input_file_directory <- "/scratch/PI/manishad/Youth_Group/GOALS/imputationProject/input/oneMinChoi/"
output_directory <- "/scratch/PI/manishad/Youth_Group/GOALS/imputationProject/output/simData3d/"

cohort_size <- 100
n_cohorts <- 10

file_list <- list.files(input_file_directory)

library(lubridate)
library(plyr)
library(scales)
library(stringr)
library(PhysicalActivity)


#define beginning and end of daytime
daytime <- c("07:00:00", "21:00:00")

#define start end dates of school year
#based on oakland public school start dates?
school_year <- c("09/01/", "05/31")

getDayOfYear <- function(.date) {
  library(lubridate)
  as.numeric( .date - mdy(paste0( "01/01/", year(.date) ) ) )
}

#load choi'ed data
class_files <- openFilesInDirectory(directory = input_file_directory, 
                                    merge = TRUE, delim_str = ",", match_string = "", header = TRUE)

#format ids
class_files$fID <- gsub("CHOI_|t.{1,}", "", class_files$loaded_file_name)

#create numeric version of wear
class_files$nonwear <- 0
class_files$nonwear[class_files$wearing_choi_vecmag == "nw"] <- 1

#generate timestamp variable
class_files$ts <- mdy(class_files$Date) + hms(class_files$Time)

#generate hour variable
class_files$hour <-   hour(hms(class_files$Time)) 

#generate season variable
class_files$start_doy <- getDayOfYear(mdy( paste0( school_year[1], year( mdy( class_files$Date) ) ) ))
class_files$end_doy <- getDayOfYear(mdy( paste0( school_year[2], year( mdy( class_files$Date) ) ) ))
class_files$doy <- getDayOfYear(mdy( class_files$Date ) )

class_files$season <- "Summer Vacation"
class_files$season[class_files$doy >= class_files$start_doy | 
                     class_files$doy <= class_files$end_doy ] <- "School Year"

class_files$ts <- as.POSIXct(class_files$ts)
#remove hours with any non-wear time
wear_hours <- ddply(class_files, .(fID, season, hour, weekday, Date), function(.subject_hour) {
  cat(.subject_hour$fID[1], "\n")
  
  if( ( max(.subject_hour$nonwear) == 0 ) & ( nrow(.subject_hour) == 60 ) ){ 
    return(.subject_hour) 
  }
  
})

wear_hours$day_hour <- paste0( wear_hours$weekday, "_",
                               str_pad(wear_hours$hour, width = 2, side = "left", pad = "0"), "_",
                               str_pad(wear_hours$doy, width = 3, side = "left", pad = "0"))

wear_hours$fID_day_hour <- paste0(wear_hours$fID, "_", wear_hours$day_hour)

wear_hours$minute <- minute(hms(wear_hours$Time))

#index who has which hours complete
wear_hours_index <- ddply(class_files, .(fID, season, hour, weekday, Date), function(.subject_hour) {
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

#sample subjects with complete data and use it to assemble a cohort
sim_duration <- system.time({
  l_ply(1:n_cohorts, .parallel = TRUE, function(.cohort_num, .cohort_size, .wear_hours_index, .wear_hours, .output_directory) {
    
    #inialize an object with all combinations of days and hours
    .day_hours <- expand.grid(weekday = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                              hour = str_pad(0:23, width = 2, side = "left", pad = "0"),
                              size = .cohort_size,
                              stringsAsFactors = FALSE)
    
    .day_hours$day_hour <- paste0(.day_hours$weekday, "_", 
                                  str_pad(.day_hours$hour, width = 2, side = "left", pad = "0") )
    
    
    #generate meta data by iterating across day-hours and sampling the a number of day-hours equal
    #to the cohort size
    .sampled_meta <- ddply(.day_hours, .(day_hour), function(..day_hour, ..sample_data) {
      # print(..day_hour)
      #subset all data with only current day-hour
      ..sample_day_hour <- ..sample_data[grepl(..day_hour$day_hour, ..sample_data$day_hour),] 
      #all should be unique bc fID_day_hour includes day of year
      ..hours <- unique(..sample_day_hour$fID_day_hour)
      #sample cohor_size from complete day-hours 
      ..tor <- data.frame(day_hour = sample(..hours, replace = TRUE, size = ..day_hour$size))
      
      return(..tor)
    }, .wear_hours_index)
    
    #generate ids for simulated subjects
    .sampled_meta$sim_id <- rep(1:.cohort_size, times = nrow(.day_hours)) 
    
    #combine into fID_day_hour
    .sampled_meta$sim_id_fID_day_hour <- paste0(.sampled_meta$sim_id, "_", .sampled_meta$day_hour)
    
    #expand meta data: hour-level becomes minute level
    .expanded_meta <- expand.grid(sim_id_fID_day_hour = .sampled_meta$sim_id_fID_day_hour,
                                  minute = 0:59)
    
    #extract fID_day_hour from sim_id_fID_day_hour
    .expanded_meta$fID_day_hour <- gsub("^[:0-9:]{1,}_", "", .expanded_meta$sim_id_fID_day_hour)
    
    #extract sim_id from sim_id_fID_day_hour
    .expanded_meta$sim_id <- gsub("_.{1,}$", "", .expanded_meta$sim_id_fID_day_hour)
    
    #make sure minutes are numerics and not Date/Time objects
    .expanded_meta$minute <- as.numeric(.expanded_meta$minute)
    .wear_hours$minute <- as.numeric(.wear_hours$minute)
    
    
    #use metadata to construct sampled data
    #by merging observed data with meta data
    .simulated_data <- merge(x = .expanded_meta,
                             y = .wear_hours,
                             by.x = c("fID_day_hour","minute"),
                             by.y = c("fID_day_hour","minute"),
                             all.x = TRUE)
    
    #write files
    .cohort_formatted <- str_pad(string = .cohort_num, width = 4, side = "left", pad = "0")
    write.table(x = .simulated_data, file = paste0(.output_directory, "sim_goals_", .cohort_formatted, ".csv"), 
                sep = ",", col.names = TRUE, row.names = FALSE)
    
  }, cohort_size, wear_hours_index, wear_hours, output_directory)
}) #end of system.time

sim_duration
sim_duration/60

#takes about 90 minutes with 16 cores on sherlock with 128GB RAM